import numpy as np

NEAP_MAX = 5.0

# Source https://www.vims.edu/research/units/labgroups/tc_tutorial/tide_analysis.php
def semidiurnal_tide(*, min_water_factor=0, max_water_factor=0, neap_factor=0.0, centered_on_hw=True):
	def compute(tide_time):
		nonlocal min_water_factor, max_water_factor
		def constituent(speed_degrees):
			interval_between_hw_and_lw = 6.1
			half_amplitude = 0.5
			constituent_phase = interval_between_hw_and_lw / 2 if centered_on_hw else 0
			speed_radians = speed_degrees * np.pi / 180
			return half_amplitude + half_amplitude * np.cos(speed_radians * tide_time - constituent_phase)

		moon_amplitude = 3.2 + 0.1 * neap_factor
		solar_amplitude = 0.3 + 0.05 * neap_factor
		moon_speed = 28.9 + 0.2 * neap_factor
		solar_speed = 30.0
		total_amplitude = moon_amplitude + solar_amplitude
		base_height = 0
		if min_water_factor > max_water_factor:
			min_water_factor = max_water_factor
		if max_water_factor > 0:
			total_amplitude *= (2.5 + 0.2 * neap_factor)/(max_water_factor + min_water_factor)
			base_height = min_water_factor + 0.5 * neap_factor
		return (base_height + moon_amplitude * constituent(moon_speed) + solar_amplitude * constituent(solar_speed)) / total_amplitude
	return compute
