import subprocess

import matplotlib.pyplot as plot
import matplotlib.patches as pc
import numpy as np


def is_dark_mode():
	"""Checks DARK/LIGHT mode of macos."""
	cmd = 'defaults read -g AppleInterfaceStyle'
	p = subprocess.Popen(cmd, stdout=subprocess.PIPE,
						 stderr=subprocess.PIPE, shell=True)
	return bool(p.communicate()[0])


def get_plot_colors():
	color_back = 'white'
	color_fore = 'black'
	color_labels = 'darkgreen'
	color_springs = 'firebrick'
	color_neaps = 'dodgerblue'
	if is_dark_mode():
		color_back = '#303030'
		color_fore = 'tan'
		color_labels = 'palegreen'
		color_springs = 'lightcoral'
		color_neaps = 'lightskyblue'
	return color_back, color_fore, color_labels, color_springs, color_neaps


def plot_tide(springs_tide_func, neaps_tide_func=None, springs_mean=0, neaps_mean=0, size_inches=(14, 7), max_low_water=9, max_high_water=15.3):
	h = lambda t: springs_tide_func(t) if neaps_tide_func is None else max(springs_tide_func(t), neaps_tide_func(t))

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
			while tide_hour > 6:
				tide_hour -= increment
				y = h(tide_hour)
				if y > line_height:
					tide_hour -= increment
					break
			intersection.append(adjust + tide_hour)
		return intersection

	# set up figure
	color_back, color_fore, color_labels, color_springs, color_neaps = get_plot_colors()
	time_range_hours = 12
	time = np.linspace(0, time_range_hours, 1000)
	fig = plot.figure(figsize=size_inches, facecolor=color_back)
	hour_step = 1 / 6
	ax = fig.add_subplot(1, 1, 1)
	ax.margins(x=0, y=0)
	ax.set_yticks([])
	ax.set_xticks([])
	ax.spines['top'].set_visible(False)
	ax.spines['right'].set_visible(False)
	ax.spines['bottom'].set_visible(False)
	ax.spines['left'].set_visible(False)

	# set up grid
	water_height_step = 1 / 10.0
	water_height_factor = 1.3
	if max_high_water > 8:
		water_height_step = 1 / 5.0
		water_height_factor = 0.8
		if max_high_water > 11:
			water_height_factor = 0.5
	curve_start = max_low_water * water_height_factor + 0.8
	if max_high_water > 10:
		curve_start += 0.5


	# draw top (HW) ticks and labels
	ax.vlines(water_height_factor * np.arange(0, max_low_water + water_height_step, water_height_step), 0, .015, linewidth=.5, colors=color_fore)
	for i in range(0, int(max_high_water) + 1):
		plot.text(water_height_factor * i - 0.08, 1 + 0.01, str(i), color=color_labels)

	# draw bottom (LW) ticks and labels
	ax.vlines(water_height_factor * np.arange(0, max_high_water + water_height_step, water_height_step), 1, 1 - .015, linewidth=.5, colors=color_fore)
	for i in range(0, int(max_low_water) + 1):
		plot.text(water_height_factor * i - 0.08, -0.03, str(i), color=color_labels)

	# draw grid horizontal lines
	h_lines_y = [n / 10.0 + 0.001 for n in range(0, 11)]
	h_lines_y[10] -= 0.0025
	ax.hlines(h_lines_y, 0, compute_ebb_intersection(heights=h_lines_y, adjust=curve_start + hour_step), colors=color_fore)

	# plot factor labels
	plot.text(curve_start + 6 + .6, 0.54, 'Factor', rotation=270, rotation_mode='anchor', color=color_labels)
	for i in range(1, 10):
		text_y = h_lines_y[i]
		plot.text(curve_start + 6 + 0.05, text_y + 0.005, format(i / 10.0, '.1f'), color=color_labels)


	# draw grid vertical lines
	v_lines_x = [n * water_height_factor for n in range(1, int(max_high_water) + 1)]
	# left-most vertical line
	ax.vlines(0.02, 0, 1, linewidth=1.5, colors=color_fore)
	# thick vertical lines
	ax.vlines(v_lines_x, compute_flood_intersection(x_axis=v_lines_x, lw_start=curve_start), 1, linewidth=1.5, colors=color_fore)
	v_lines_x = [(n + 0.5) * water_height_factor for n in range(0, int(max_high_water))]
	# thin vertical lines
	ax.vlines(v_lines_x, compute_flood_intersection(x_axis=v_lines_x, lw_start=curve_start), 1, linewidth=.5, colors=color_fore)
	ax.vlines(curve_start + np.arange(hour_step, 12 - hour_step, hour_step), 0, .015, linewidth=.5, colors=color_fore)

	# plot grid labels
	plot.text(0 - .06, 1 + .05, 'H.W. heights m', color=color_labels)
	plot.text(0 + .1, 0.59, 'CHART DATUM', rotation=270, rotation_mode='anchor', color=color_labels)
	plot.text(0 - .06, 0 - .07, 'L.W. heights m', color=color_labels)
	labels = ['LW', '-5h', '-4h', '-3h', '-2h', '-1h', 'HW', '+1h', '+2h', '+3h', '+4h', '+5h', 'LW']

	# draw tide hours marks and boxes to write in
	box_width = 1
	box_height = 0.06
	for i in range(0, 13):
		plot.text(curve_start + i - 0.3, -0.03, labels[i], color=color_labels)
		ax.add_patch(pc.Rectangle((curve_start + i - box_width / 1.7, -box_height - 0.05), box_width, box_height, edgecolor=color_fore, fill=False))

	# draw tide hours vertical lines
	# thick lines
	ax.vlines(curve_start + np.arange(1, 12 + hour_step, 6 * hour_step), 0, [h(x) for x in range(1, 13)], linewidth=1.5, colors=color_fore)
	# thin lines
	ax.vlines(curve_start + np.arange(1 + 3 * hour_step, 11 + 3 * hour_step, 6 * hour_step), 0, [h(x + 3 * hour_step) for x in range(1, 11)], linewidth=.5, colors=color_fore)

	# draw neaps curve
	if neaps_tide_func is not None:
		neaps_tide = neaps_tide_func(time)
		ax.plot(curve_start + time, neaps_tide, label=f"Neaps {format(neaps_mean, '.1f')}m", color=color_neaps, linestyle='dashed')

	# draw springs curve
	springs_tide = springs_tide_func(time)
	ax.plot(curve_start + time, springs_tide, label=f"Springs {format(springs_mean, '.1f')}m", color=color_springs)

	# draw legend and title
	ax.set_facecolor(color_back)
	plot.title('Fictional Port Tidal Curve', y=1.1, fontsize=14, fontweight='bold')
	plot.legend(title='MEAN RANGES', title_fontsize=12, facecolor=color_back, labelcolor=color_fore)

	# show the whole plot
	plot.show()

