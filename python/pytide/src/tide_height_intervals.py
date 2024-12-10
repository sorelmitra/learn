import datetime

from src.lib import debug
from src.tide_find import find_this_or_next_water, find_previous_tide, find_next_tide
from src.tide_height_find import find_height_time_between_tides
from src.tide_tables import TideHeight, TideDay


class TidePointInTime:
	def __init__(self, *, day_number: int, time: datetime.datetime):
		self.day_number = day_number
		self.time = time

	def print(self):
		print(f"Day number: {self.day_number}, time: {self.time}")


class TideInterval:
	def __init__(self, *, start: TidePointInTime, end: TidePointInTime):
		self.start = start
		self.end = end

	def print(self, initial_date):
		"""
		Prints the interval in the format [start_time - end_time].
		Uses the initial_date and the day_number of the start and end times
		to determine the date of the start and end times.
		"""
		start_time = datetime.datetime.combine(initial_date, self.start.time.time())
		start_time += datetime.timedelta(days=self.start.day_number - 1)

		end_time = datetime.datetime.combine(initial_date, self.end.time.time())
		end_time += datetime.timedelta(days=self.end.day_number - 1)

		print(f"[{start_time} - {end_time}]")


def determine_water_height_interval(life_cycle,
									tide_days: list[TideDay],
									day_number: int, tide_number: int,
									height_to_find: float,
									tide_duration: datetime.datetime = None):
	"""
	Determines the interval during which the tide height is at least or at most a given height,
	depending on the life_cycle parameter:
	- For HW, the interval refers to the time when the tide height is at least the given height.
	- For LW, the interval refers to the time when the tide height is at most the given height.

	Parameters:
	- life_cycle: The type of tide to find (HW or LW).
	- tide_days: List of TideDay objects.
	- day_number: The day number (1-based index) to start the search from.
	- tide_number: The tide number (1-based index) within the start day to start the search from.
	- height_to_find: The height to find between the two tides.
	- tide_duration: The typical duration of a tide cycle, used at the start and end of the tidal data.

	Returns:
	- A TideInterval object representing the interval during which the tide height is at least or at most the given height.
	"""

	# cw stands for 'center water', which is one of LW or HW, as indicated
	# in the parameter
	cw, cw_day_number, cw_tide_number = find_this_or_next_water(
		tide_days=tide_days, life_cycle=life_cycle,
		day_number=day_number, tide_number=tide_number)

	prev_tide, prev_tide_day_number, _ = find_previous_tide(
		tide_days=tide_days, day_number=day_number, tide_number=cw_tide_number)
	next_tide, next_tide_day_number, _ = find_next_tide(
		tide_days=tide_days, day_number=day_number, tide_number=cw_tide_number)

	if cw is None:
		raise ValueError("No high water found in the provided data.")
	if prev_tide is None and next_tide is None:
		raise ValueError("No previous or next tide found in the provided data.")

	if tide_duration is None:
		tide_duration = datetime.timedelta(hours=6, minutes=20)

	cw_full_time = datetime.datetime.combine(datetime.datetime.now(), cw.time)
	if next_tide is None:
		# We are at the end of the tidal data
		# Set a fake next tide, based on a typical tide duration
		# We copy the previous tide values, except for the time
		next_tide_day_number = prev_tide_day_number + 1
		next_tide = TideHeight(
			time=(cw_full_time + tide_duration).time(),
			height=prev_tide.height,
			life_cycle=prev_tide.type,
			neap_level=prev_tide.neap_level,
			compute_height=prev_tide.compute_height)
		debug(f"Fake next tide: {next_tide.time}")

	if prev_tide is None:
		# We are at the start of the tidal data
		# Set a fake previous tide, based on a typical tide duration
		# We copy the next tide values, except for the time
		prev_tide_day_number = next_tide_day_number - 1
		prev_tide = TideHeight(
			time=(cw_full_time - tide_duration).time(),
			height=next_tide.height,
			life_cycle=next_tide.type,
			neap_level=next_tide.neap_level,
			compute_height=next_tide.compute_height)
		debug(f"Fake previous tide: {prev_tide.time}")

	# Find the first time corresponding to the given height
	start_day_number, start_time = find_height_time_between_tides(
		height_to_find=height_to_find,
		first_tide_info=(prev_tide, prev_tide_day_number),
		second_tide_info=(cw, day_number),
		compute_height_for_hw=(life_cycle == TideHeight.HW),
		compute_height_for_first_tide=False)

	# Find the second time corresponding to the given height
	end_day_number, end_time = find_height_time_between_tides(
		height_to_find=height_to_find,
		first_tide_info=(cw, day_number),
		second_tide_info=(next_tide, next_tide_day_number),
		compute_height_for_hw=(life_cycle == TideHeight.HW),
		compute_height_for_first_tide=True)

	return TideInterval(
		start=TidePointInTime(day_number=start_day_number, time=start_time),
		end=TidePointInTime(day_number=end_day_number, time=end_time)
	)


def determine_min_water_height_interval(tide_days: list[TideDay],
										day_number: int, tide_number: int,
										height_to_find: float,
										tide_duration: datetime.datetime = None):
	"""
	Determines the interval during which the tide height is at least a given height.

	Parameters:
	- tide_days: List of TideDay objects.
	- day_number: The day number (1-based index) to start the search from.
	- tide_number: The tide number (1-based index) within the start day to start the search from.
	- height_to_find: The height to find between the two tides.
	- tide_duration: The typical duration of a tide cycle, used at the start and end of the tidal data.

	Returns:
	- A TideInterval object representing the interval during which the tide height is at least the given height.
	"""
	return determine_water_height_interval(
		life_cycle=TideHeight.HW,
		tide_days=tide_days,
		day_number=day_number,
		tide_number=tide_number,
		height_to_find=height_to_find,
		tide_duration=tide_duration
	)


def determine_max_water_height_intervals(tide_days: list[TideDay],
										 day_number: int, tide_number: int,
										 height_to_find: float,
										 tide_duration: datetime.datetime = None):
	"""
	Determines the intervals during which the tide height is at most a given height.

	Parameters:
	- tide_days: List of TideDay objects.
	- day_number: The day number (1-based index) to start the search from.
	- tide_number: The tide number (1-based index) within the start day to start the search from.
	- height_to_find: The height to find between the two tides.
	- tide_duration: The typical duration of a tide cycle, used at the start and end of the tidal data.

	Returns:
	- A list of two TideInterval objects representing the intervals during which the tide height is at most the given height:
	- The first interval is before the high water (HW) tide, and corresponds to the LW that precedes the HW.
	- The second interval is after the HW tide, and corresponds to the LW that follows the HW.
	"""

	_, hw_day_number, hw_tide_number = find_this_or_next_water(
		tide_days=tide_days, life_cycle=TideHeight.HW,
		day_number=day_number, tide_number=tide_number)

	_, prev_lw_day_number, prev_lw_tide_number = find_previous_tide(
		tide_days=tide_days, day_number=hw_day_number, tide_number=hw_tide_number)

	interval_before_hw = determine_water_height_interval(
		life_cycle=TideHeight.LW,
		tide_days=tide_days,
		day_number=prev_lw_day_number,
		tide_number=prev_lw_tide_number,
		height_to_find=height_to_find,
		tide_duration=tide_duration
	)

	_, next_lw_day_number, next_lw_tide_number = find_next_tide(
		tide_days=tide_days, day_number=hw_day_number, tide_number=hw_tide_number)

	interval_after_hw = determine_water_height_interval(
		life_cycle=TideHeight.LW,
		tide_days=tide_days,
		day_number=next_lw_day_number,
		tide_number=next_lw_tide_number,
		height_to_find=height_to_find,
		tide_duration=tide_duration
	)

	return [interval_before_hw, interval_after_hw]
