import datetime

import pytest

from src.tide_height_intervals import determine_min_water_height_interval, \
	determine_max_water_height_intervals
from src.tide_model import semidiurnal_tide
from src.tide_tables import TideDay, TideHeight


@pytest.fixture
def sample_tide_days():
	compute_height = semidiurnal_tide(
		min_water_factor=2,
		max_water_factor=5,
		neap_factor=3.82
	)
	return [
		TideDay(
			tide_date=datetime.date(2024, 1, 1),
			heights=[
				TideHeight(
					time=datetime.time(5, 50), height=5.8,
					life_cycle=TideHeight.HW, compute_height=compute_height,
				),
				TideHeight(
					time=datetime.time(12, 10), height=3.0,
					life_cycle=TideHeight.LW, compute_height=compute_height
				),
				TideHeight(
					time=datetime.time(18, 30), height=5.9,
					life_cycle=TideHeight.HW, compute_height=compute_height
				),
			],
		),
		TideDay(
			tide_date=datetime.date(2024, 1, 2),
			heights=[
				TideHeight(
					time=datetime.time(3, 40), height=2.6,
					life_cycle=TideHeight.LW, compute_height=compute_height
				),
				TideHeight(
					time=datetime.time(10, 0), height=6.4,
					life_cycle=TideHeight.HW, compute_height=compute_height,
				),
				TideHeight(
					time=datetime.time(16, 20), height=2.7,
					life_cycle=TideHeight.LW, compute_height=compute_height
				),
				TideHeight(
					time=datetime.time(22, 40), height=6.4,
					life_cycle=TideHeight.HW, compute_height=compute_height
				),
			],
		),
		TideDay(
			tide_date=datetime.date(2024, 1, 3),
			heights=[
				TideHeight(
					time=datetime.time(4, 50), height=2.6,
					life_cycle=TideHeight.LW, compute_height=compute_height
				),
				TideHeight(
					time=datetime.time(11, 10), height=6.4,
					life_cycle=TideHeight.HW, compute_height=compute_height,
				),
				TideHeight(
					time=datetime.time(17, 30), height=2.7,
					life_cycle=TideHeight.LW, compute_height=compute_height
				),
				TideHeight(
					time=datetime.time(23, 50), height=6.4,
					life_cycle=TideHeight.HW, compute_height=compute_height
				),
			],
		),
		TideDay(
			tide_date=datetime.date(2024, 1, 4),
			heights=[
				TideHeight(
					time=datetime.time(6, 10), height=2.6,
					life_cycle=TideHeight.LW, compute_height=compute_height
				),
				TideHeight(
					time=datetime.time(12, 30), height=6.4,
					life_cycle=TideHeight.HW, compute_height=compute_height,
				),
				TideHeight(
					time=datetime.time(18, 50), height=2.7,
					life_cycle=TideHeight.LW, compute_height=compute_height
				)
			],
		),
	]


def test_find_interval_of_minimum_water_height(sample_tide_days):
	interval = determine_min_water_height_interval(
		tide_days=sample_tide_days, day_number=3, tide_number=1,
		height_to_find=4.3)
	assert interval.start.day_number == 3
	assert interval.start.time.time() == datetime.time(7, 40)
	assert interval.end.day_number == 3
	assert interval.end.time.time() == datetime.time(14, 23)


def test_find_interval_of_minimum_water_height_span_next_day(sample_tide_days):
	interval = determine_min_water_height_interval(
		tide_days=sample_tide_days, day_number=2, tide_number=3,
		height_to_find=4.3)
	assert interval.start.day_number == 2
	assert interval.start.time.time() == datetime.time(19, 10)
	assert interval.end.day_number == 3
	assert interval.end.time.time() == datetime.time(1, 48)


def test_find_interval_of_minimum_water_height_hw_is_last(sample_tide_days):
	interval = determine_min_water_height_interval(
		tide_days=sample_tide_days, day_number=3, tide_number=3,
		height_to_find=4.3)
	assert interval.start.day_number == 3
	assert interval.start.time.time() == datetime.time(20, 20)
	assert interval.end.day_number == 4
	assert interval.end.time.time() == datetime.time(3, 3)


def test_find_interval_of_minimum_water_height_hw_is_first(sample_tide_days):
	interval = determine_min_water_height_interval(
		tide_days=sample_tide_days, day_number=1, tide_number=1,
		height_to_find=4.3)
	assert interval.start.day_number == 1
	assert interval.start.time.time() == datetime.time(2, 20)
	assert interval.end.day_number == 1
	assert interval.end.time.time() == datetime.time(9, 3)


def test_find_intervals_of_maximum_water_height(sample_tide_days):
	intervals = determine_max_water_height_intervals(
		tide_days=sample_tide_days, day_number=2, tide_number=2,
		height_to_find=4.3)
	assert len(intervals) == 2
	# Interval before HW
	interval_before_hw = intervals[0]
	assert interval_before_hw.start.day_number == 1
	assert interval_before_hw.start.time.time() == datetime.time(23, 10)
	assert interval_before_hw.end.day_number == 2
	assert interval_before_hw.end.time.time() == datetime.time(6, 30)
	# Interval after HW
	interval_after_hw = intervals[1]
	assert interval_after_hw.start.day_number == 2
	assert interval_after_hw.start.time.time() == datetime.time(13, 13)
	assert interval_after_hw.end.day_number == 2
	assert interval_after_hw.end.time.time() == datetime.time(19, 10)


def test_find_intervals_of_maximum_water_height_lw_is_last(sample_tide_days):
	intervals = determine_max_water_height_intervals(
		tide_days=sample_tide_days, day_number=4, tide_number=2,
		height_to_find=4.3)
	assert len(intervals) == 2
	# Interval before HW (on previous day)
	interval_before_hw = intervals[0]
	assert interval_before_hw.start.day_number == 4
	assert interval_before_hw.start.time.time() == datetime.time(3, 3)
	assert interval_before_hw.end.day_number == 4
	assert interval_before_hw.end.time.time() == datetime.time(9, 0)
	# Interval after HW
	interval_after_hw = intervals[1]
	assert interval_after_hw.start.day_number == 4
	assert interval_after_hw.start.time.time() == datetime.time(15, 43)
	assert interval_after_hw.end.day_number == 4
	assert interval_after_hw.end.time.time() == datetime.time(21, 40)

