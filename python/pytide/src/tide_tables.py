import datetime


class TideHeight:
	def __init__(self, *,
				 compute_func,
				 day_of_month, neap_level,
				 hw_height, lw_height,
				 hw_time=datetime.time(hour=0, minute=0),
				 lw_time=datetime.time(hour=6, minute=0)):
		self.day = day_of_month
		self.neap_level = neap_level
		self.hw_height = hw_height
		self.hw_time = hw_time
		self.lw_height = lw_height
		self.lw_time = lw_time
		self.compute_func = compute_func
