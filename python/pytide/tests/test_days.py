import datetime

from src.tide_model import NEAP_MAX
from src.tide_tables import reset_day, TideHeight, generate_tide_days, compute_max_hw, compute_springs_mean, \
	compute_max_lw, compute_neaps_mean


def test_generate_one_height():
	tide_days = generate_tide_days(start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=1)
	assert len(tide_days) == 1

	tide_day = tide_days[0]
	assert tide_day.date.day == 1
	assert tide_day.neap_level == NEAP_MAX
	assert len(tide_day.heights) == 1

	tide_height = tide_day.heights[0]
	assert tide_height.time.hour == 3
	assert tide_height.time.minute == 10
	assert tide_height.height > 0
	assert tide_height.type == TideHeight.HW


def test_generate_one_day():
	delta = datetime.timedelta(hours=6, minutes=20)
	tide_days = generate_tide_days(start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=4, delta=delta)
	assert len(tide_days) == 1

	tide_day = tide_days[0]
	tide_day.print()

	assert tide_day.date.day == 1
	assert tide_day.neap_level == NEAP_MAX
	assert len(tide_day.heights) == 4

	tide_height = tide_day.heights[0]
	assert tide_height.time.hour == 3
	assert tide_height.time.minute == 10
	assert tide_height.height > 0
	assert tide_height.type == TideHeight.HW

	tide_height = tide_day.heights[1]
	assert tide_height.time.hour == 9
	assert tide_height.time.minute == 30
	assert tide_height.height > 0
	assert tide_height.height < tide_day.heights[0].height
	assert tide_height.type == TideHeight.LW

	tide_height = tide_day.heights[2]
	assert tide_height.time.hour == 15
	assert tide_height.time.minute == 50
	assert tide_height.height > 0
	assert tide_height.height > tide_day.heights[1].height
	assert tide_height.type == TideHeight.HW

	tide_height = tide_day.heights[3]
	assert tide_height.time.hour == 22
	assert tide_height.time.minute == 10
	assert tide_height.height > 0
	assert tide_height.height < tide_day.heights[2].height
	assert tide_height.type == TideHeight.LW


def test_generate_one_day_from_lw():
	delta = datetime.timedelta(hours=6, minutes=20)
	tide_days = generate_tide_days(start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=2, delta=delta, life_cycle=TideHeight.LW)
	assert len(tide_days) == 1

	tide_day = tide_days[0]
	assert tide_day.date.day == 1
	assert tide_day.neap_level == NEAP_MAX
	assert len(tide_day.heights) == 2

	tide_height = tide_day.heights[0]
	assert tide_height.time.hour == 3
	assert tide_height.time.minute == 10
	assert tide_height.height > 0
	assert tide_height.type == TideHeight.LW

	tide_height = tide_day.heights[1]
	assert tide_height.time.hour == 9
	assert tide_height.time.minute == 30
	assert tide_height.height > 0
	assert tide_height.height > tide_day.heights[0].height
	assert tide_height.type == TideHeight.HW


def test_generate_one_cycle_from_neaps_to_springs():
	delta = datetime.timedelta(hours=6, minutes=20)
	tide_days = generate_tide_days(start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=0, cycle_length=8, delta=delta)
	assert len(tide_days) == 8

	old_neap_level = None
	for tide in tide_days:
		# check that neap levels are decreasing
		print(f"Neap level for day {tide.date.day} is {tide.neap_level}")
		if old_neap_level is None:
			assert tide.neap_level == NEAP_MAX
			old_neap_level = tide.neap_level
		else:
			assert tide.neap_level < old_neap_level

		# check that high water levels are increasing
		prev_tide_value = None
		for tide_value in tide.heights:
			if tide_value.type == TideHeight.HW:
				if prev_tide_value is None:
					prev_tide_value = tide_value
				else:
					print(f"HW height for day {tide.date.day}, time {tide_value.time.strftime('%H%M')} is {format(tide_value.height, '.2f')}, previous was time {prev_tide_value.time.strftime('%H%M')} {format(prev_tide_value.height, '.2f')}")
					assert tide_value.height > prev_tide_value.height + 0.01
					prev_tide_value = tide_value

		# check that low water levels are decreasing
		prev_tide_value = None
		for tide_value in tide.heights:
			if tide_value.type == TideHeight.LW:
				if prev_tide_value is None:
					prev_tide_value = tide_value
				else:
					print(f"LW height for day {tide.date.day}, time {tide_value.time.strftime('%H%M')} is {format(tide_value.height, '.2f')}, previous was time {prev_tide_value.time.strftime('%H%M')} {format(prev_tide_value.height, '.2f')}")
					assert tide_value.height < prev_tide_value.height - 0.01
					prev_tide_value = tide_value
		print()

	high_waters = [t for t in tide_days[7].heights if t.type == TideHeight.HW]
	tide_day_7_hw = high_waters[len(high_waters) - 1]
	low_waters = [t for t in tide_days[7].heights if t.type == TideHeight.LW]
	tide_day_7_lw = low_waters[len(low_waters) - 1]
	tide_day_0_hw = [t for t in tide_days[0].heights if t.type == TideHeight.HW][0]
	tide_day_0_lw = [t for t in tide_days[0].heights if t.type == TideHeight.LW][0]
	assert compute_max_hw(tide_days) == tide_day_7_hw.height
	assert compute_max_lw(tide_days) == tide_day_0_lw.height
	assert abs(compute_springs_mean(tide_days) - (tide_day_7_hw.height - tide_day_7_lw.height)) < 0.1
	assert abs(compute_neaps_mean(tide_days) - (tide_day_0_hw.height - tide_day_0_lw.height)) < 0.1


def test_generate_one_cycle_various_water_height_factors():
	delta = datetime.timedelta(hours=6, minutes=20)
	common_params = dict(start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=0, cycle_length=8, delta=delta)

	tide_days = generate_tide_days(**common_params)
	[t.print() for t in tide_days]
	assert compute_max_hw(tide_days) > compute_max_lw(tide_days) + 1
	assert compute_springs_mean(tide_days) > compute_neaps_mean(tide_days) + 0.5
