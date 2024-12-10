from datetime import datetime


def date_subtract_in_minutes(from_date=None, date_to_subtract=None):
	"""
    Calculates the difference between two datetime objects in minutes.

    Parameters:
        from_date (datetime): The datetime to subtract from. If None, the current datetime is used.
        date_to_subtract (datetime): The datetime to subtract. If None, the current datetime is used.

    Returns:
        int: The difference between the two datetimes in minutes. If the first datetime is earlier
             than the second, the result will be negative.

    Note:
        If either `from_date` or `date_to_subtract` is None, this function uses the current datetime
        for the missing value(s), which effectively calculates the difference from now for a given datetime
        or the difference between now and now (which will be 0).
    """
	# Ensure default to current datetime if any datetime is not provided
	if from_date is None:
		from_date = datetime.now()
	if date_to_subtract is None:
		date_to_subtract = datetime.now()

	# Calculate the difference between the two datetimes
	difference = from_date - date_to_subtract

	# Convert the difference to minutes
	minutes = difference.seconds // 60

	# Adjust minutes for any days difference (note: difference.days could be negative)
	if difference.days < 0:
		minutes = minutes + difference.days * 24 * 60

	return minutes
