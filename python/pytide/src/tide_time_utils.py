import datetime
import random

from src.tide_tables import TideHeight, TideDay


def generate_random_time_between_tides(*, tide_days: list[TideDay],
									   day_number: int, tide_number: int):
	day_index = day_number - 1
	tide_index = tide_number - 1

	# Retrieve the specific TideDay
	tide_day = tide_days[day_index]
	# Retrieve the tide height entry
	current_tide = tide_day.heights[tide_index]

	# Determine next tide
	if tide_index + 1 < len(tide_day.heights):
		# The next tide is later the same day
		next_tide = tide_day.heights[tide_index + 1]
		next_tide_day = tide_day
	elif day_index + 1 < len(tide_days):
		# The next tide is the first tide of the next day
		next_tide_day = tide_days[day_index + 1]
		next_tide = next_tide_day.heights[0]
	else:
		# If it's the last tide of the last day in the array, use a default end time (e.g., 23:59)
		next_tide_day = tide_day  # No next day, so default to the current day
		next_tide = TideHeight(time=datetime.time(23, 59))

	# Convert tide times to datetime.datetime objects for easy manipulation
	current_tide_datetime = datetime.datetime.combine(tide_day.date, current_tide.time)
	next_tide_datetime = datetime.datetime.combine(next_tide_day.date, next_tide.time)

	# Generate a random datetime between the current tide and the next
	if next_tide_datetime > current_tide_datetime:
		random_datetime = current_tide_datetime + (
					next_tide_datetime - current_tide_datetime) * random.random()
	else:
		# If next tide datetime is not greater (due to defaulting to 23:59), adjust logic as needed
		# For simplicity, we default to current_tide_datetime for now
		random_datetime = current_tide_datetime

	# Return the time part of the random datetime
	return random_datetime.time()


def timedelta_to_twelve_based_tide_hours(td: datetime.timedelta):
	"""
	Converts a datetime.timedelta object to a floating point number representing tide hours.
	Tide hours run between 0 and 12, with 0 representing the first low water of the interval, 6 representing the high water, and 12 representing the second low water.

	Parameters:
	- td: datetime.timedelta object.

	Returns:
	- float: The number of tide 12-based hours represented by the timedelta, including fractional parts.
	"""
	total_seconds = td.total_seconds()
	hours = total_seconds / 3600  # Convert seconds to hours

	# Convert to 12-based tide hours
	hours += 6
	if hours < 0:
		hours += 6
	if hours > 12:
		hours -= 12
	return hours


