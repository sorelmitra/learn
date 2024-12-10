import datetime

import pytest

from src.tide_closest_hw import ClosestHighWater, find_closest_high_water
from src.tide_tables import TideDay, TideHeight


@pytest.fixture
def sample_tide_days():
	return [
		TideDay(
			tide_date=datetime.date(2024, 1, 1),
			heights=[
				TideHeight(time=datetime.time(2, 0), height=1.5, life_cycle=TideHeight.LW),
				TideHeight(time=datetime.time(8, 0), height=2.5, life_cycle=TideHeight.HW),
				TideHeight(time=datetime.time(14, 0), height=1.0, life_cycle=TideHeight.LW),
				TideHeight(time=datetime.time(22, 0), height=2.0, life_cycle=TideHeight.HW),
			]
		),
		TideDay(
			tide_date=datetime.date(2024, 1, 2),
			heights=[
				TideHeight(time=datetime.time(4, 0), height=2.5, life_cycle=TideHeight.HW),
				TideHeight(time=datetime.time(9, 0), height=1.0, life_cycle=TideHeight.LW),
				TideHeight(time=datetime.time(15, 0), height=2.0, life_cycle=TideHeight.HW),
				TideHeight(time=datetime.time(21, 0), height=1.5, life_cycle=TideHeight.LW),
			]
		),
		TideDay(
			tide_date=datetime.date(2024, 1, 3),
			heights=[
				TideHeight(time=datetime.time(2, 0), height=2.5, life_cycle=TideHeight.HW),
				TideHeight(time=datetime.time(10, 0), height=1.0, life_cycle=TideHeight.LW),
				TideHeight(time=datetime.time(16, 0), height=2.0, life_cycle=TideHeight.HW),
				TideHeight(time=datetime.time(22, 0), height=1.5, life_cycle=TideHeight.LW),
			]
		),
		TideDay(
			tide_date=datetime.date(2024, 1, 4),
			heights=[
				TideHeight(time=datetime.time(3, 30), height=2.5, life_cycle=TideHeight.LW),
				TideHeight(time=datetime.time(9, 50), height=1.0, life_cycle=TideHeight.HW),
				TideHeight(time=datetime.time(16, 10), height=2.0, life_cycle=TideHeight.LW),
				TideHeight(time=datetime.time(22, 30), height=1.5, life_cycle=TideHeight.HW),
			]
		),
	]


def test_closest_hw_same_day(sample_tide_days):
	given_time = datetime.time(12, 0)
	closest_hw = find_closest_high_water(
		tide_days=sample_tide_days, day_number=2, given_time=given_time)
	assert closest_hw.time == datetime.time(15, 0)
	assert closest_hw.day_number == 2
	assert closest_hw.tide_number == 3
	assert closest_hw.hw_diff == datetime.timedelta(days=0) - datetime.timedelta(hours=3)


def test_closest_hw_same_day_four(sample_tide_days):
	given_time = datetime.time(16, 24)
	closest_hw = find_closest_high_water(
		tide_days=sample_tide_days, day_number=4, given_time=given_time)
	assert closest_hw.time == datetime.time(22, 30)
	assert closest_hw.day_number == 4
	assert closest_hw.tide_number == 4
	assert closest_hw.hw_diff == datetime.timedelta(days=0) - datetime.timedelta(hours=6, minutes=6)


def test_closest_hw_previous_day(sample_tide_days):
	given_time = datetime.time(0, 30)
	closest_hw = find_closest_high_water(
		tide_days=sample_tide_days, day_number=2, given_time=given_time)
	assert closest_hw.time == datetime.time(22, 0)
	assert closest_hw.day_number == 1
	assert closest_hw.tide_number == 4
	assert closest_hw.hw_diff == datetime.timedelta(hours=2, minutes=30)


def test_closest_hw_next_day(sample_tide_days):
	given_time = datetime.time(23, 30)
	closest_hw = find_closest_high_water(
		tide_days=sample_tide_days, day_number=2, given_time=given_time)
	assert closest_hw.time == datetime.time(2, 0)
	assert closest_hw.day_number == 3
	assert closest_hw.tide_number == 1
	assert closest_hw.hw_diff == datetime.timedelta(days=0) - datetime.timedelta(hours=2, minutes=30)


def test_no_hw_tides(sample_tide_days):
	# Modify the fixture for this test to ensure no HW tides exist
	for day in sample_tide_days:
		for tide in day.heights:
			tide.type = TideHeight.LW  # Set all to low water

	given_time = datetime.time(13, 13)
	with pytest.raises(ValueError):
		find_closest_high_water(
			tide_days=sample_tide_days, day_number=2, given_time=given_time)


def test_hw_positive_hour_difference():
	td = datetime.timedelta(hours=2, minutes=45)
	hw_pos = ClosestHighWater(
		time=datetime.time(14, 45), day_number=1, tide_number=2, hw_diff=td)
	assert hw_pos.get_hw_hour_string() == "HW+3"


def test_hw_negative_hour_and_thirty_minutes_difference():
	td = datetime.timedelta(hours=-1, minutes=-30)
	hw_pos = ClosestHighWater(
		time=datetime.time(10, 30), day_number=1, tide_number=2, hw_diff=td)
	assert hw_pos.get_hw_hour_string() == "HW-2"


def test_hw_exactly_thirty_minutes_difference():
	td = datetime.timedelta(minutes=30)
	hw_pos = ClosestHighWater(
		time=datetime.time(11, 30), day_number=1, tide_number=2, hw_diff=td)
	assert hw_pos.get_hw_hour_string() == "HW"


def test_hw_boundary_condition_hour_bump():
	td = datetime.timedelta(minutes=31)
	hw_pos = ClosestHighWater(
		time=datetime.time(10, 31), day_number=1, tide_number=2, hw_diff=td)
	assert hw_pos.get_hw_hour_string() == "HW+1"


def test_hw_zero_hour_difference():
	td = datetime.timedelta(hours=0)
	hw_pos = ClosestHighWater(
		time=datetime.time(12, 0), day_number=1, tide_number=2, hw_diff=td)
	assert hw_pos.get_hw_hour_string() == "HW"
