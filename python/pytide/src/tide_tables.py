from src.tide_model import NEAP_MAX


class TideHeight:
	LW = 'low water'
	HW = 'high water'

	# Constructs a tide height object
	# @param time: the time of the tide height, a datetime.time() object
	# @param height: the height of the tide, a float number representing meters
	# @param life_cycle: the type of tide height (high or low water)
	def __init__(self, *, time, height, life_cycle):
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
	def __init__(self, *, compute_func, tide_date, neap_level, heights):
		self.date = tide_date
		self.neap_level = neap_level
		self.compute_func = compute_func
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
