import datetime

from src.tide_model import NEAP_MAX
from src.tide_tables import reset_day, TideHeight, generate_tide_days, compute_max_hw, compute_springs_mean, \
	compute_max_lw, compute_neaps_mean


def test_generate_one_height():
	tide_days = generate_tide_days(a_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=1)
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
	tide_days = generate_tide_days(a_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=4, delta=delta)
	assert len(tide_days) == 1

	tide_day = tide_days[0]
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
	tide_days = generate_tide_days(a_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=2, delta=delta, life_cycle=TideHeight.LW)
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
	tide_days = generate_tide_days(a_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)), heights_count=0, cycle_length=8, delta=delta)
	assert len(tide_days) == 8

	assert tide_days[0].neap_level == NEAP_MAX
	tide_day_0_hw = [t for t in tide_days[0].heights if t.type == TideHeight.HW][0]
	tide_day_0_lw = [t for t in tide_days[0].heights if t.type == TideHeight.LW][0]

	assert tide_days[1].neap_level < tide_days[0].neap_level
	tide_day_1_hw = [t for t in tide_days[1].heights if t.type == TideHeight.HW][0]
	tide_day_1_lw = [t for t in tide_days[1].heights if t.type == TideHeight.LW][0]
	assert tide_day_1_hw.height > tide_day_0_hw.height
	assert tide_day_1_lw.height < tide_day_0_lw.height

	assert tide_days[2].neap_level < tide_days[1].neap_level
	tide_day_2_hw = [t for t in tide_days[2].heights if t.type == TideHeight.HW][0]
	tide_day_2_lw = [t for t in tide_days[2].heights if t.type == TideHeight.LW][0]
	assert tide_day_2_hw.height > tide_day_1_hw.height
	assert tide_day_2_lw.height < tide_day_1_lw.height

	assert tide_days[3].neap_level < tide_days[2].neap_level
	tide_day_3_hw = [t for t in tide_days[3].heights if t.type == TideHeight.HW][0]
	tide_day_3_lw = [t for t in tide_days[3].heights if t.type == TideHeight.LW][0]
	assert tide_day_3_hw.height > tide_day_2_hw.height
	assert tide_day_3_lw.height < tide_day_2_lw.height

	assert tide_days[4].neap_level < tide_days[3].neap_level
	tide_day_4_hw = [t for t in tide_days[4].heights if t.type == TideHeight.HW][0]
	tide_day_4_lw = [t for t in tide_days[4].heights if t.type == TideHeight.LW][0]
	assert tide_day_4_hw.height > tide_day_3_hw.height
	assert tide_day_4_lw.height < tide_day_3_lw.height

	assert tide_days[5].neap_level < tide_days[4].neap_level
	tide_day_5_hw = [t for t in tide_days[5].heights if t.type == TideHeight.HW][0]
	tide_day_5_lw = [t for t in tide_days[5].heights if t.type == TideHeight.LW][0]
	assert tide_day_5_hw.height > tide_day_4_hw.height
	assert tide_day_5_lw.height < tide_day_4_lw.height

	assert tide_days[6].neap_level < tide_days[5].neap_level
	tide_day_6_hw = [t for t in tide_days[6].heights if t.type == TideHeight.HW][0]
	tide_day_6_lw = [t for t in tide_days[6].heights if t.type == TideHeight.LW][0]
	assert tide_day_6_hw.height > tide_day_5_hw.height
	assert tide_day_6_lw.height < tide_day_5_lw.height

	assert tide_days[7].neap_level == 0.0
	tide_day_7_hw = [t for t in tide_days[7].heights if t.type == TideHeight.HW][0]
	tide_day_7_lw = [t for t in tide_days[7].heights if t.type == TideHeight.LW][0]
	assert tide_day_7_hw.height > tide_day_6_hw.height
	assert tide_day_7_lw.height < tide_day_6_lw.height

	assert compute_max_hw(tide_days) == tide_day_7_hw.height
	assert compute_max_lw(tide_days) == tide_day_0_lw.height
	assert compute_springs_mean(tide_days) == tide_day_7_hw.height - tide_day_7_lw.height
	assert compute_neaps_mean(tide_days) == tide_day_0_hw.height - tide_day_0_lw.height
