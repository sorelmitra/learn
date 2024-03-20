import datetime
import random

from src.tide_model import semidiurnal_tide, NEAP_MAX
from src.tide_plot import plot_tide
from src.tide_tables import generate_tide_days, reset_day, compute_springs_mean, compute_max_hw, compute_neaps_mean, \
	compute_max_lw

if __name__ == '__main__':
	tide_hour = 1 + 11 * random.random()
	tide_hour_per_hw = tide_hour - 6

	cycle_length = random.randint(7, 9)
	start_date = reset_day() + datetime.timedelta(hours=3, minutes=10)
	tide_days = generate_tide_days(
		start_date=start_date,
		cycle_length=cycle_length,
		delta=datetime.timedelta(hours=6, minutes=20)
	)

	print(f"Month: {start_date.strftime('%B')}, generating a full cycle of {cycle_length} days length")
	print()
	[tide.print() for tide in tide_days]

	day = random.randint(0, len(tide_days) - 1)
	tide = tide_days[day]

	hw_sign = '' if tide_hour_per_hw < 0 else '+'
	hw_string = f"{hw_sign}{format(tide_hour_per_hw, '.1f')}" if abs(tide_hour_per_hw) >= 0.1 else ''
	tide_height_string = format(tide.compute_height(tide_hour), '.1f')
	print(f"Tide height for day {day + 1} at HW{hw_string}: {tide_height_string} m")

	plot_tide(
		springs_tide_func=(semidiurnal_tide()),
		springs_mean=compute_springs_mean(tide_days),
		max_high_water=compute_max_hw(tide_days) + 1,
		neaps_tide_func=(semidiurnal_tide(neap_factor=NEAP_MAX)),
		neaps_mean=compute_neaps_mean(tide_days),
		max_low_water=compute_max_lw(tide_days) + 1
	)

