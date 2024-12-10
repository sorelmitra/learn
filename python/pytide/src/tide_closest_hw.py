import datetime

from src.lib import debug, debug_func
from src.tide_tables import TideHeight


class ClosestHighWater:
	"""
	Represents the closest high water time in relation to a given time.

	Attributes:
	- time: `datetime.time` object representing the tide time.
	- day_number: The day number in the tide_days array (1-based) of the tide time.
	- tide_number: The tide number within the day (1-based) of the tide time.
	"""

	def __init__(self, *, time: datetime.time, day_number: int, tide_number: int,
				 hw_diff: datetime.timedelta):
		self.time = time
		self.day_number = day_number
		self.tide_number = tide_number
		self.hw_diff = hw_diff

	def print(self):
		print(f"Closest HW is at {self.time.strftime('%H%M')}, tide hour {self.get_hw_hour_string()}, tide number: {self.tide_number}")

	def get_hw_hour_string(self):
		total_seconds = int(self.hw_diff.total_seconds())
		hours = total_seconds // 3600  # Divide by 3600 to get hours
		minutes = (total_seconds % 3600) // 60  # Use modulus by 3600 to get remaining seconds, then divide by 60 to get minutes
		if minutes > 30:
			hours += 1
		sign = ''
		if hours > 0:
			sign = '+'
		hours_str = f"{sign}{hours}"
		if hours == 0:
			hours_str = ''
		return f"HW{hours_str}"


def find_closest_high_water(*, tide_days, day_number, given_time):
	min_time_diff = datetime.timedelta.max
	closest_hw_time = None
	day_index = day_number - 1

	# Helper function to update the closest HW tide based on a new candidate
	def update_closest_hw(*, candidate_time, candidate_day_number, candidate_tide_number, day_step):
		nonlocal closest_hw_time, min_time_diff
		day_of_reference = datetime.date.today()
		if day_step < 0:
			day_of_reference -= datetime.timedelta(days=1)
		elif day_step > 0:
			day_of_reference += datetime.timedelta(days=1)
		candidate_datetime = datetime.datetime.combine(day_of_reference, candidate_time)
		given_datetime = datetime.datetime.combine(datetime.date.today(), given_time)
		time_diff = given_datetime - candidate_datetime
		abs_time_diff = abs(time_diff)
		debug(f"Candidate: {candidate_datetime}, given: {given_datetime}, time diff: {abs_time_diff}")

		if abs_time_diff < min_time_diff:
			min_time_diff = abs_time_diff
			closest_hw_time = ClosestHighWater(
				time=candidate_time,
				day_number=candidate_day_number,
				tide_number=candidate_tide_number,
				hw_diff=time_diff)
			debug('Chosen')

	# Search for the closest HW tide in the current, previous, and next day
	for index_offset in (0, -1, 1):
		current_day_index = day_index + index_offset
		if 0 <= current_day_index < len(tide_days):
			n = len(tide_days[current_day_index].heights)
			if index_offset < 0:
				start = n - 1
				stop = -1
				step = -1
			else:
				start = 0
				stop = n
				step = 1
			for k in range(start, stop, step):
				tide = tide_days[current_day_index].heights[k]
				debug()
				debug_func(tide_days[current_day_index].print)
				if tide.type == TideHeight.HW:
					update_closest_hw(
						candidate_time=tide.time, candidate_day_number=current_day_index + 1,
						candidate_tide_number=tide_days[current_day_index].heights.index(tide) + 1,
						day_step=index_offset)

	if closest_hw_time is not None:
		return closest_hw_time
	else:
		raise ValueError("No high water found in the provided data.")


