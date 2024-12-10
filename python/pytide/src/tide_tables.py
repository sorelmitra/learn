import datetime

from src.lib import debug, debug_func
from src.tide_model import NEAP_MAX, semidiurnal_tide


def reset_day(d=datetime.datetime.now()):
	return d.replace(day=1, hour=0, minute=0, second=0, microsecond=0)


class TideHeight:
	LW = 'low water'
	HW = 'high water'

	# Constructs a tide height object
	# @param time: the time of the tide height, a datetime.time() object
	# @param height: the height of the tide, a float number representing meters
	# @param life_cycle: the type of tide height (high or low water)
	# @param neap_level: the neap level of the tide, a float number between 0 (springs) and NEAP_MAX (neaps)
	# @param compute_height: the function that computes the tide height
	def __init__(self, *,
				 time=datetime.datetime.now().time(), height=0.0, life_cycle=LW,
				 neap_level=0.0,
				 compute_height=lambda time: 0.0):
		self.time = time
		self.height = height
		self.type = life_cycle
		self.neap_level = neap_level
		self.compute_height = compute_height

	def print(self):
		print(f"{self.time.strftime('%H%M')} {format(self.height, '.1f')}")


class TideDay:
	# Constructs a tide day object
	# @param compute_func: the function that computes the tide height
	# @param tide_date: the date of the tide, a datetime.date() object
	# @param neap_level: the neap level of the tide, a float number between 0 (springs) and NEAP_MAX (neaps)
	# @param heights: the tide heights for the day, a list of TideHeight objects; the constructor will sort them by time
	def __init__(self, *, tide_date: datetime.date, heights: list[TideHeight], neap_level=0.0):
		self.date = tide_date
		self.neap_level = neap_level
		self.heights = heights
		# sort heights by time
		self.heights.sort(key=lambda x: x.time)

	def print(self):
		neap_level_string = f"neap level {format(self.neap_level, '.2f')}"
		if self.neap_level == NEAP_MAX:
			neap_level_string = 'neaps'
		elif self.neap_level == 0:
			neap_level_string = 'springs'
		print(f"{self.date.day}, {self.date.strftime('%a')} ({neap_level_string})")
		[tide_height.print() for tide_height in self.heights]
		print()


class NeapDirection:
	def __init__(self, *, cycle_length: int,
				 tide_range_should_increase: bool,
				 start_days_after_neaps: int):
		self.tide_range_should_increase = tide_range_should_increase
		self.step = 0
		if cycle_length > 1:
			self.step = self.get_max() / (cycle_length * 2 - 1)
		self.start_from_offset = 0
		if start_days_after_neaps is not None:
			self.start_from_offset = start_days_after_neaps * 2
		debug(f"step: {self.step}, start_from_offset: {self.start_from_offset}, tide_range_should_increase: {self.tide_range_should_increase}")

	@staticmethod
	def get_max():
		return NEAP_MAX

	def get_start(self):
		if self.start_from_offset > 0:
			# start with 'offset' steps down from max
			return self.get_max() - self.start_from_offset * self.step

		if self.tide_range_should_increase:
			return self.get_max()
		return 0.0

	def get_next(self, neap_level: float):
		if self.tide_range_should_increase:
			return self._decrement(neap_level)

		return self._increment(neap_level)

	def _increment(self, neap_level: float):
		next_value = neap_level + self.step
		if next_value > self.get_max() - 0.05:
			next_value = self.get_max()
		return next_value

	def _decrement(self, neap_level: float):
		next_value = neap_level - self.step
		if next_value < 0.05:
			next_value = 0.0
		return next_value

	def is_at_end(self, neap_level: float):
		if self.tide_range_should_increase:
			return neap_level == 0.0
		return neap_level == self.get_max()

	def reverse(self):
		self.tide_range_should_increase = not self.tide_range_should_increase
		debug(f"step: {self.step}, start_from_offset: {self.start_from_offset}, tide_range_should_increase: {self.tide_range_should_increase}")
		pass


# Generates a list of tide days with heights and times, starting from
# a specified date and neap level.
# As the tide heights are added, neap level progresses in the indicated
# direction (towards springs or neaps).
# As a Springs or Neaps is reached, direction is reversed and the cycle
# continues for the number of days to generate.
#
# @param start_date: the date of the tide, a datetime object
# @param heights_count: the number of heights to generate
# @param days_count: the number of days to generate; if missing, it defaults to cycle_length
# @param cycle_length: the number of days in a neaps - springs cycle; if present, it overrides
# heights_count and generates a full cycle
# @param time_delta: the time difference between heights
# @param start_life_cycle: the type of tide height (high or low water) to use when stating
# @param min_water_factor: the minimum water height factor, it affects generated low tide values
# @param max_water_factor: the maximum water height factor, it affects generated high tide values
# @param go_towards_springs: a boolean value that indicates
# if the tide range should initially increase (i.e. progress towards springs),
# or decrease (i.e. progress towards neaps)
def generate_tide_days(start_date=datetime.datetime.now(),
					   days_count=0, heights_count=1, cycle_length=0,
					   time_delta=datetime.timedelta(hours=6, minutes=0),
					   start_life_cycle=TideHeight.HW,
					   min_water_factor=2, max_water_factor=5, go_towards_springs=True,
					   start_days_after_neaps=None,
					   should_vary_water_factors=False):

	tide_days = []
	tide_heights = []
	old_a_date = start_date
	current_life_cycle = start_life_cycle
	reversal_count = 0  # Counter for the number of reversals
	increase_factors = True  # Direction of adjustment, True for increase, False for decrease

	if days_count == 0:
		days_count = cycle_length
	if days_count > 0:
		heights_count = days_count * 4

	neap_dir = NeapDirection(cycle_length=cycle_length,
							 tide_range_should_increase=go_towards_springs,
							 start_days_after_neaps=start_days_after_neaps)
	neap_level = neap_dir.get_start()

	compute_current_height = semidiurnal_tide(
		min_water_factor=min_water_factor,
		max_water_factor=max_water_factor,
		neap_factor=neap_level
	)

	neaps_cycle_count = 0
	old_neap_level = neap_level
	tide_hour = 6 if current_life_cycle == TideHeight.HW else 0
	old_low_tide_hour = 12 if tide_hour == 6 else 0
	day_index = 0
	for _ in range(0, heights_count):
		tide_height = TideHeight(
			time=start_date.time(),
			height=compute_current_height(tide_hour),
			life_cycle=current_life_cycle,
			neap_level=neap_level,
			compute_height = compute_current_height
		)
		tide_heights.append(tide_height)
		current_life_cycle = TideHeight.HW if current_life_cycle == TideHeight.LW else TideHeight.LW

		if tide_hour == 0:
			tide_hour = 6
		elif tide_hour == 6:
			tide_hour = 12 if old_low_tide_hour == 0 else 0
			old_low_tide_hour = tide_hour
		else:
			tide_hour = 6

		start_date = start_date + time_delta

		neaps_cycle_count += 1
		if neaps_cycle_count == 2:
			neap_level = neap_dir.get_next(neap_level)
			compute_current_height = semidiurnal_tide(
				min_water_factor=min_water_factor,
				max_water_factor=max_water_factor,
				neap_factor=neap_level
			)
			neaps_cycle_count = 0
			if neap_dir.is_at_end(neap_level):
				old_neap_level = neap_level
				neap_dir.reverse()

				if should_vary_water_factors:
					reversal_count += 1

					# Adjust the factors based on the current phase and reversal count
					if reversal_count % 3 == 0:  # Change direction after every 3 reversals
						increase_factors = not increase_factors

					adjustment_min_abs = 0.04
					adjustment_min = adjustment_min_abs if increase_factors else -adjustment_min_abs
					adjustment_max_abs = 0.06
					adjustment_max = adjustment_max_abs if increase_factors else -adjustment_max_abs
					min_water_factor += adjustment_min
					max_water_factor += adjustment_max
			debug(f"neap_level: {neap_level:.2f}, start_date: {start_date}")

		if start_date.day != old_a_date.day:
			day_neap_level = neap_level
			# old_neap_level is used for day 1 to make sure we start from the
			# actual Neaps or Springs level
			# also used when Neaps or Springs is in the middle of the generated array
			if old_neap_level is not None:
				day_neap_level = old_neap_level
				old_neap_level = None
			tide_day = TideDay(
				tide_date=old_a_date.date(),
				neap_level=day_neap_level,
				heights=tide_heights
			)
			tide_days.append(tide_day)
			tide_heights = []
			old_a_date = start_date
			day_index += 1
			debug(f"Adding new day #{day_index}, neap_level: {day_neap_level:.2f}, values")
			debug_func(tide_day.print)

		if (days_count > 0) and (day_index == days_count):
			break

	if (days_count == 0 or len(tide_days) < days_count) and len(tide_heights) > 0:
		tide_day = TideDay(
			tide_date=start_date.date(),
			neap_level=neap_level,
			heights=tide_heights
		)
		debug(f"Adding last day, neap_level: {neap_level:.2f}, values")
		debug_func(tide_day.print)
		tide_days.append(tide_day)

	return tide_days


def compute_max_hw(tide_days: list[TideDay]):
	max_hw = 0
	for tide_day in tide_days:
		for tide_height in tide_day.heights:
			if tide_height.type == TideHeight.HW and tide_height.height > max_hw:
				max_hw = tide_height.height
	return max_hw


def compute_max_lw(tide_days: list[TideDay]):
	max_lw = 0
	for tide_day in tide_days:
		for tide_height in tide_day.heights:
			if tide_height.type == TideHeight.LW and tide_height.height > max_lw:
				max_lw = tide_height.height
	return max_lw


def compute_springs_mean(tide_days: list[TideDay]):
	springs_mean_values = []
	for tide_day in tide_days:
		if tide_day.neap_level == 0:
			n = len(tide_day.heights)
			height1 = tide_day.heights[n - 1].height
			height2 = tide_day.heights[n - 2].height
			springs_mean_values.append(abs(height1 - height2))
			break
	if len(springs_mean_values) == 0:
		return None
	return sum(springs_mean_values) / len(springs_mean_values)


def compute_neaps_mean(tide_days: list[TideDay]):
	neaps_mean_values = []
	for tide_day in tide_days:
		if tide_day.neap_level == NEAP_MAX:
			height1 = tide_day.heights[0].height
			height2 = tide_day.heights[1].height
			neaps_mean_values.append(abs(height1 - height2))
			break
	if len(neaps_mean_values) == 0:
		return None
	return sum(neaps_mean_values) / len(neaps_mean_values)
