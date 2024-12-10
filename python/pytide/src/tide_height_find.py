import datetime

from src.lib import debug, debug_func
from src.tide_tables import TideHeight


def find_height_time_between_tides(*, height_to_find: float,
								   first_tide_info: (TideHeight, int),
								   second_tide_info: (TideHeight, int),
								   compute_height_for_hw: bool,
								   compute_height_for_first_tide: bool):
	"""
	Finds the time between two tides when the tide height is equal to a given height.

	Parameters:
	- height_to_find: The height to find between the two tides.
	- first_tide_info: A tuple (tide_height, day_number) representing the first tide.
	- second_tide_info: A tuple (tide_height, day_number) representing the second tide.
	- compute_height_for_hw: A boolean indicating whether to compute the height for high water (HW).
	- compute_height_for_first_tide: A boolean indicating whether to compute the height for the first tide.

	Returns:
	- A tuple (day_number, time) representing the day number and time of the tide height.`
	"""

	first_tide, first_tide_day_number = first_tide_info
	second_tide, second_tide_day_number = second_tide_info
	start_date = datetime.date.today()
	end_date = start_date + datetime.timedelta(
		days=second_tide_day_number - first_tide_day_number)
	debug(f"Start date: {start_date}, end date: {end_date}, First tide day number: {first_tide_day_number}, Second tide day number: {second_tide_day_number}")

	start_time = datetime.datetime.combine(start_date, first_tide.time)
	end_time = datetime.datetime.combine(end_date, second_tide.time)

	if compute_height_for_first_tide:
		if compute_height_for_hw:
			start_time_12_hours = 6
			end_time_12_hours = 12
		else:
			start_time_12_hours = 0
			end_time_12_hours = 6
	else:
		if compute_height_for_hw:
			start_time_12_hours = 0
			end_time_12_hours = 6
		else:
			start_time_12_hours = 6
			end_time_12_hours = 12

	tide_for_calculations = second_tide
	if compute_height_for_first_tide:
		tide_for_calculations = first_tide

	start_height = tide_for_calculations.compute_height(start_time_12_hours)
	end_height = tide_for_calculations.compute_height(end_time_12_hours)

	debug(f"First tide")
	debug_func(first_tide.print)
	debug(f"Second tide")
	debug_func(second_tide.print)

	debug(f"Start time: {start_time}, end time: {end_time}")
	debug(f"12-hours-based: Start time: {start_time_12_hours:.1f}, end time: {end_time_12_hours:.1f}")
	debug(f"Start height: {start_height:.1f}, end height: {end_height:.1f}")

	while (end_time - start_time) > datetime.timedelta(minutes=1):  # Precision threshold
		mid_time = start_time + (end_time - start_time) / 2
		mid_time_12_hours = start_time_12_hours + (end_time_12_hours - start_time_12_hours) / 2
		debug(f"Start time: {start_time}, end time: {end_time}, mid time: {mid_time}")
		debug(f"12-hours-based start time: {start_time_12_hours:.1f}, end time: {end_time_12_hours:.1f}, mid time: {mid_time_12_hours:.1f}")
		mid_height = tide_for_calculations.compute_height(mid_time_12_hours)
		debug(f"Start height: {start_height:.1f}, end height: {end_height:.1f}, mid height: {mid_height:.1f}")

		if (mid_height < height_to_find and start_height < end_height) or (
				mid_height > height_to_find and start_height > end_height):
			start_time = mid_time
			start_time_12_hours = mid_time_12_hours
		else:
			end_time = mid_time
			end_time_12_hours = mid_time_12_hours

	# Reset precision to minutes
	start_time = start_time.replace(second=0, microsecond=0)

	start_day_number = first_tide_day_number
	if start_time.day > start_date.day:
		start_day_number += start_time.day - start_date.day
	debug(f"Start day number: {start_day_number}, Start time: {start_time}, End time: {end_time}")

	return start_day_number, start_time


