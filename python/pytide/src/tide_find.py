from src.tide_tables import TideHeight, TideDay


def find_previous_tide(*, tide_days: list[TideDay], day_number: int, tide_number: int):
	"""
	Finds the previous high water (HW) or low water (LW) tide in a list of TideDay objects.

	Parameters:
	- tide_days: List of TideDay objects.
	- day_number: The day number (1-based index) to start the search from.
	- tide_number: The tide number (1-based index) within the start day to start the search from.

	Returns:
	- A tuple (tide_height, day_number, tide_number) indicating the tide height object, and the
	day and tide number of the previous HW or LW.
	"""

	# Adjust to 0-based index for iteration
	day_index = day_number - 1
	tide_index = tide_number - 2  # Start search from the tide before the given tide_number

	while day_index >= 0:
		tide_day = tide_days[day_index]
		while tide_index >= 0:
			tide = tide_day.heights[tide_index]
			if tide.type in [TideHeight.HW, TideHeight.LW]:
				return tide, day_index + 1, tide_index + 1  # Convert back to 1-based index for result
			tide_index -= 1
		# Move to the previous day and start from the last tide of that day
		day_index -= 1
		if day_index >= 0:  # Check to prevent index error on the last iteration
			tide_index = len(tide_days[day_index].heights) - 1

	return None, 0, 0  # Return None if no previous HW or LW is found


def find_next_tide(*, tide_days: list[TideDay], day_number: int, tide_number: int):
	"""
	Finds the next high water (HW) or low water (LW) tide in a list of TideDay objects.

	Parameters:
	- tide_days: List of TideDay objects.
	- day_number: The day number (1-based index) to start the search from.
	- tide_number: The tide number (1-based index) within the start day to start the search from.

	Returns:
	- A tuple (tide_height, day_number, tide_number) indicating the tide height object, and the
	day and tide number of the next HW or LW.
	"""
	day_index = day_number - 1  # Convert to 0-based index for iteration
	tide_index = tide_number  # Start search from the tide immediately after the given tide_number

	while day_index < len(tide_days):
		tide_day = tide_days[day_index]
		while tide_index < len(tide_day.heights):
			tide = tide_day.heights[tide_index]
			if tide.type in [TideHeight.HW, TideHeight.LW]:
				return tide, day_index + 1, tide_index + 1  # Convert back to 1-based index for result
			tide_index += 1
		# Move to the next day and start from the first tide of that day
		day_index += 1
		tide_index = 0

	return None, 0, 0  # Return None if no next HW or LW is found


def find_this_or_next_water(*, tide_days: list[TideDay], life_cycle: str,
							day_number: int, tide_number: int):
	"""
	Finds the next high water (HW) or low water (LW) tide in a list of TideDay objects,
	starting from the given day and tide number.
	If the tide at the given day and tide number is not of the specified type, the next tide of the specified type is found.  Else, the tide at the given day and tide number is returned.

	Parameters:
	- tide_days: List of TideDay objects.
	- life_cycle: The type of tide to find (HW or LW).
	- day_number: The day number (1-based index) to start the search from.
	- tide_number: The tide number (1-based index) within the start day to start the search from.

	Returns:
	- A tuple (tide_height, day_number, tide_number) indicating the tide height object, and the day and tide number of the next HW or LW.
	"""

	specified_tide = tide_days[day_number - 1].heights[tide_number - 1]
	# cw stands for 'center water', which is one of LW or HW, as indicated
	# in the parameter
	if specified_tide.type == life_cycle:
		cw, cw_day_number, cw_tide_number = specified_tide, day_number, tide_number
	else:
		cw, cw_day_number, cw_tide_number = find_next_tide(
			tide_days=tide_days, day_number=day_number, tide_number=tide_number)
		if cw.type != life_cycle:
			raise ValueError(f"No {life_cycle} found in the provided data.")
	return cw, cw_day_number, cw_tide_number


