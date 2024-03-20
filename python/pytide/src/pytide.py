import random

from src.tide_model import semidiurnal_tide, NEAP_MAX
from src.tide_plot import plot_tide
from src.tide_tables import TideHeight

tide_factors = dict(min_water_factor=2, max_water_factor=5)
neap_level = NEAP_MAX
compute_springs_height = semidiurnal_tide(**tide_factors)
compute_neaps_height = semidiurnal_tide(**tide_factors, neap_factor=neap_level)

neaps_lw = compute_neaps_height(0)
neaps_hw = compute_neaps_height(6)

springs_lw = compute_springs_height(0)
springs_hw = compute_springs_height(6)

# TODO: Make a generative function that goes both ways neaps <-> springs
tide_heights = []
tide_cycle_length = random.randint(7, 9)
step = neap_level / (tide_cycle_length - 1)
current_neap_level = neap_level
for k in range(0, tide_cycle_length):
	# compute tide height based on neap level
	compute_current_height = semidiurnal_tide(**tide_factors, neap_factor=current_neap_level)

	# store tide height and function
	tide = TideHeight(
		compute_func=compute_current_height,
		day_of_month=k + 1,
		neap_level=current_neap_level,
		hw_height=compute_current_height(6),
		lw_height=compute_current_height(0),
	)
	tide_heights.append(tide)

	# print tide height for the day
	neap_level_string = f", neap level {format(current_neap_level, '.2f')}"
	if tide.neap_level == neap_level:
		neap_level_string = ' (neaps)'
	elif tide.neap_level == 0:
		neap_level_string = ' (springs)'
	print(f"Day {tide.day}{neap_level_string}")
	print(f"HW={format(tide.hw_height, '.1f')}")
	print(f"LW={format(tide.lw_height, '.1f')}")
	print()

	# next neap level
	current_neap_level = current_neap_level - step
	# adjust very small neap levels to 0, to make sure we hit Springs
	if current_neap_level < 0.05:
		current_neap_level = 0.0

a_tide_hour = 1 + 11 * random.random()
tide_hour_per_hw = a_tide_hour - 6
current_day = random.randint(0, len(tide_heights) - 1)
current_tide = tide_heights[current_day]
hw_sign = '' if tide_hour_per_hw < 0 else '+'
hw_string = f"{hw_sign}{format(tide_hour_per_hw, '.1f')}" if abs(tide_hour_per_hw) >= 0.1 else ''
tide_height_string = format(current_tide.compute_func(a_tide_hour), '.1f')
print(f"Tide height for day {current_day + 1} at HW{hw_string}: {tide_height_string} m")

plot_tide(
	springs_tide_func=(semidiurnal_tide()),
	springs_mean=springs_hw - springs_lw,
	max_high_water=springs_hw + 1,
	neaps_tide_func=(semidiurnal_tide(neap_factor=neap_level)),
	neaps_mean=neaps_hw - neaps_lw,
	max_low_water=springs_lw + 1
)
