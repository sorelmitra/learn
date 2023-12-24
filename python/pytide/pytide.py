import random

import matplotlib.pyplot as plot
import matplotlib.patches as pc
import numpy as np


# Source https://www.vims.edu/research/units/labgroups/tc_tutorial/tide_analysis.php
def semidiurnal_tide(neap_level=0, centered_on_hw=True):
	def with_height_variation(height_variation=0):
		def compute(tide_time):
			my_neap_level = neap_level  # Python stupidity

			def constituent(speed_degrees):
				interval_between_hw_and_lw = 6.1
				half_amplitude = 0.5
				constituent_phase = interval_between_hw_and_lw / 2 if centered_on_hw else 0
				speed_radians = speed_degrees * np.pi / 180
				return half_amplitude + half_amplitude * np.cos(speed_radians * tide_time - constituent_phase)

			max_neap_level = 5
			if my_neap_level > max_neap_level:
				my_neap_level = max_neap_level
			if my_neap_level < 0:
				my_neap_level = 0
			moon_amplitude = 3.2
			solar_amplitude = (0.3 + 0.15 * my_neap_level) * (-1 if my_neap_level > 0 else 1)
			moon_speed = 28.9
			solar_speed = 30.0
			total_amplitude = moon_amplitude + solar_amplitude if height_variation == 0 else 1
			neap_variation = 0 if height_variation == 0 else -0.09 * my_neap_level + (0.2 * my_neap_level if tide_time == 0 else 0)
			return (height_variation + neap_variation + moon_amplitude * constituent(moon_speed) + solar_amplitude * constituent(solar_speed)) / total_amplitude
		return compute
	return with_height_variation


def plot_tide(springs_tide_func, neaps_tide_func=None, springs_mean=0, neaps_mean=0, size_inches=(14, 7), max_low_water=9, max_high_water=15.3):
	h = lambda t: max(springs_tide_func(t), neaps_tide_func(t))

	def compute_flood_intersection(x_axis, lw_start=0.0):
		intersection = []
		tide_hour = x_axis[0]
		for x in x_axis:
			if x < lw_start - 0.5:
				intersection.append(0)
			elif x < lw_start:
				intersection.append(0.1)
			else:
				tide_height = h(tide_hour) + 0.1
				y = tide_height
				n = y * 10.0
				y = y if np.ceil(n) - n < 0.01 else np.ceil(n) / 10.0
				y = y if y - tide_height > 0.1 else y + 0.1
				y = y if 1 - y >= 0.03 else 1 - 0.03
				intersection.append(y)
				tide_hour += 1
		return intersection

	def compute_ebb_intersection(heights, adjust=0.0):
		intersection = []
		tide_hour = 12.3
		increment = 1/10.0
		for line_height in heights:
			while True:
				tide_hour -= increment
				y = h(tide_hour)
				if y > line_height:
					tide_hour -= increment
					break
			intersection.append(adjust + tide_hour)
		return intersection

	time_range_hours = 12
	time = np.linspace(0, time_range_hours, 1000)
	fig = plot.figure(figsize=size_inches)
	hour_step = 1 / 6
	ax = fig.add_subplot(1, 1, 1)
	ax.margins(x=0, y=0)
	ax.set_yticks([])
	ax.set_xticks([])
	ax.spines['top'].set_visible(False)
	ax.spines['right'].set_visible(False)
	ax.spines['bottom'].set_visible(False)
	ax.spines['left'].set_visible(False)
	water_height_step = 1 / 10.0
	water_height_factor = 1.3
	if max_high_water > 5:
		water_height_step = 1 / 5.0
		water_height_factor = 1
	curve_start = max_low_water * water_height_factor + 0.8
	if max_high_water > 10:
		curve_start += 0.5
	ax.vlines(water_height_factor * np.arange(0, max_low_water + water_height_step, water_height_step), 0, .015, linewidth=.5, colors='black')
	ax.vlines(water_height_factor * np.arange(0, max_high_water + water_height_step, water_height_step), 1, 1 - .015, linewidth=.5, colors='black')
	for i in range(0, int(max_low_water) + 1):
		plot.text(water_height_factor * i - 0.08, -0.03, str(i), color='darkgreen')
	for i in range(0, int(max_high_water) + 1):
		plot.text(water_height_factor * i - 0.08, 1 + 0.01, str(i), color='darkgreen')
	h_lines_y = [n / 10.0 + 0.001 for n in range(0, 11)]
	ax.hlines(h_lines_y, 0, compute_ebb_intersection(heights=h_lines_y, adjust=curve_start + hour_step), colors='black')
	for i in range(1, 10):
		text_y = h_lines_y[i]
		plot.text(curve_start + 6 + 0.05, text_y + 0.005, format(i / 10.0, '.1f'), color='darkgreen')
	v_lines_x = [n * water_height_factor for n in range(1, int(max_high_water) + 1)]
	ax.vlines(0.02, 0, 1, linewidth=1.5, colors='black')
	ax.vlines(v_lines_x, compute_flood_intersection(x_axis=v_lines_x, lw_start=curve_start), 1, linewidth=1.5, colors='black')
	v_lines_x = [(n + 0.5) * water_height_factor for n in range(0, int(max_high_water))]
	ax.vlines(v_lines_x, compute_flood_intersection(x_axis=v_lines_x, lw_start=curve_start), 1, linewidth=.5, colors='black')
	ax.vlines(curve_start + np.arange(hour_step, 12 - hour_step, hour_step), 0, .015, linewidth=.5, colors='black')
	plot.text(0 - .06, 1 + .05, 'H.W. heights m', color='darkgreen')
	plot.text(0 + .1, 0.59, 'CHART DATUM', rotation=270, rotation_mode='anchor', color='darkgreen')
	plot.text(0 - .06, 0 - .07, 'L.W. heights m', color='darkgreen')
	labels = ['LW', '-5h', '-4h', '-3h', '-2h', '-1h', 'HW', '+1h', '+2h', '+3h', '+4h', '+5h', 'LW']
	box_width = 1
	box_height = 0.06
	for i in range(0, 13):
		plot.text(curve_start + i - 0.3, -0.03, labels[i], color='darkgreen')
		ax.add_patch(pc.Rectangle((curve_start + i - box_width / 1.7, -box_height - 0.05), box_width, box_height, edgecolor='black', fill=False))
	plot.text(curve_start + 6 + .6, 0.54, 'Factor', rotation=270, rotation_mode='anchor', color='darkgreen')
	ax.vlines(curve_start + np.arange(1, 12 + hour_step, 6 * hour_step), 0, [h(x) for x in range(1, 13)], linewidth=1.5, colors='black')
	ax.vlines(curve_start + np.arange(1 + 3 * hour_step, 11 + 3 * hour_step, 6 * hour_step), 0, [h(x + 3 * hour_step) for x in range(1, 11)], linewidth=.5, colors='black')
	if neaps_tide_func is not None:
		neaps_tide = neaps_tide_func(time)
		ax.plot(curve_start + time, neaps_tide, label=f"Neaps {format(neaps_mean, '.1f')}m", color='dodgerblue', linestyle='dashed')
	springs_tide = springs_tide_func(time)
	ax.plot(curve_start + time, springs_tide, label=f"Springs {format(springs_mean, '.1f')}m", color='firebrick')
	# plot.title('Fictional Port Tidal Curve')
	plot.legend()
	plot.show()


def get_height_variation():
	return 1


springs = semidiurnal_tide()
springs_curve = springs(0)
springs_height = springs(get_height_variation())
neaps = semidiurnal_tide(neap_level=5)
neaps_curve = neaps(0)
neaps_height = neaps(get_height_variation())

print('Neaps')
neaps_hw = neaps_height(6)
neaps_lw = neaps_height(0)
print(f"HW={format(neaps_hw, '.1f')}")
print(f"LW={format(neaps_lw, '.1f')}")

print('Springs')
springs_hw = springs_height(6)
springs_lw = springs_height(0)
print(f"HW={format(springs_hw, '.1f')}")
print(f"LW={format(springs_lw, '.1f')}")

a_tide_hour = 1 + 11 * random.random()
tide_hour_per_hw = a_tide_hour - 6
print(f"Tide height at HW{'' if tide_hour_per_hw < 0 else '+'}{format(tide_hour_per_hw, '.1f')}: {format(springs_height(a_tide_hour), '.1f')} m")

plot_tide(
	springs_tide_func=springs_curve,
	springs_mean=springs_hw - springs_lw,
	max_high_water=springs_hw + 1,
	neaps_tide_func=neaps_curve,
	neaps_mean=neaps_hw - neaps_lw,
	max_low_water=springs_lw + 1
)
