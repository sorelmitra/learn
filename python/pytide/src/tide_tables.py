import datetime

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
	def __init__(self, *, time=datetime.datetime.now().time(), height=0.0, life_cycle=LW):
		self.time = time
		self.height = height
		self.type = life_cycle

	def print(self):
		print(f"{self.time.strftime('%H%M')} {format(self.height, '.1f')}")


class TideDay:
	# Constructs a tide day object
	# @param compute_func: the function that computes the tide height
	# @param tide_date: the date of the tide, a datetime.date() object
	# @param neap_level: the neap level of the tide, a float number between 0 (springs) and NEAP_MAX (neaps)
	# @param heights: the tide heights for the day, a list of TideHeight objects; the constructor will sort them by time
	def __init__(self, *, compute_height=lambda time: 0.0, tide_date=datetime.datetime.now().date(), neap_level=0.0, heights=[TideHeight()]):
		self.date = tide_date
		self.neap_level = neap_level
		self.compute_height = compute_height
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


# Generates a list of tide days
# @param a_date: the date of the tide, a datetime object
# @param heights_count: the number of heights to generate
# @param cycle_length: the number of days in a neaps - springs cycle; if present, it overrides heights_count and generates a full cycle
# @param delta: the time difference between heights
# @param life_cycle: the type of tide height (high or low water)
def generate_tide_days(start_date=datetime.datetime.now(), heights_count=1, cycle_length=0,
					   delta=datetime.timedelta(hours=6, minutes=0), life_cycle=TideHeight.HW):

	neap_level = NEAP_MAX
	compute_current_height = semidiurnal_tide(
		min_water_factor=2, max_water_factor=5, neap_factor=neap_level)

	tide_days = []
	tide_heights = []
	old_a_date = start_date
	current_life_cycle = life_cycle

	step = 0
	if cycle_length > 1:
		step = neap_level / (cycle_length * 2 - 1)
	if cycle_length > 0:
		heights_count = cycle_length * 4

	neaps_cycle_count = 0
	old_neap_level = neap_level
	for _ in range(0, heights_count):
		tide_hour = 6 if current_life_cycle == TideHeight.HW else 0
		tide_height = TideHeight(
			time=start_date.time(),
			height=compute_current_height(tide_hour),
			life_cycle=current_life_cycle
		)
		tide_heights.append(tide_height)
		current_life_cycle = TideHeight.HW if current_life_cycle == TideHeight.LW else TideHeight.LW

		neaps_cycle_count += 1
		if neaps_cycle_count == 2:
			neap_level = neap_level - step
			if neap_level < 0.05:
				neap_level = 0.0
			compute_current_height = semidiurnal_tide(
				min_water_factor=2, max_water_factor=5, neap_factor=neap_level)
			neaps_cycle_count = 0

		start_date = start_date + delta

		if start_date.day != old_a_date.day:
			day_neap_level = neap_level
			if len(tide_days) < 1:
				day_neap_level = old_neap_level
				old_neap_level = neap_level
			tide_day = TideDay(
				compute_height=compute_current_height,
				tide_date=old_a_date.date(),
				neap_level=day_neap_level,
				heights=tide_heights
			)
			tide_days.append(tide_day)
			tide_heights = []
			old_a_date = start_date

	if (cycle_length == 0 or len(tide_days) < cycle_length) and len(tide_heights) > 0:
		tide_day = TideDay(
			compute_height=compute_current_height,
			tide_date=start_date.date(),
			neap_level=neap_level,
			heights=tide_heights
		)
		tide_days.append(tide_day)

	return tide_days


def compute_max_hw(tide_days):
	max_hw = 0
	for tide_day in tide_days:
		for tide_height in tide_day.heights:
			if tide_height.type == TideHeight.HW and tide_height.height > max_hw:
				max_hw = tide_height.height
	return max_hw


def compute_max_lw(tide_days):
	max_lw = 0
	for tide_day in tide_days:
		for tide_height in tide_day.heights:
			if tide_height.type == TideHeight.LW and tide_height.height > max_lw:
				max_lw = tide_height.height
	return max_lw


def compute_springs_mean(tide_days):
	springs_mean_values = []
	for tide_day in tide_days:
		if tide_day.neap_level == 0:
			height1 = tide_day.heights[0].height
			height2 = tide_day.heights[1].height
			springs_mean_values.append(abs(height1 - height2))
			break
	return sum(springs_mean_values) / len(springs_mean_values)


def compute_neaps_mean(tide_days):
	neaps_mean_values = []
	for tide_day in tide_days:
		if tide_day.neap_level == NEAP_MAX:
			height1 = tide_day.heights[0].height
			height2 = tide_day.heights[1].height
			neaps_mean_values.append(abs(height1 - height2))
			break
	return sum(neaps_mean_values) / len(neaps_mean_values)
