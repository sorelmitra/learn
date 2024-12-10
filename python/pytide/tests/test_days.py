import datetime

from src.tide_model import NEAP_MAX
from src.tide_tables import reset_day, TideHeight, generate_tide_days, \
	compute_max_hw, compute_springs_mean, \
	compute_max_lw, compute_neaps_mean
from tests.libtest import systest_get_hw, systest_get_lw, \
	systest_get_mean_ht, check_water_levels


def test_generate_one_height():
	tide_days = generate_tide_days(
		start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)),
		heights_count=1)
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
	tide_days = generate_tide_days(
		start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)),
		heights_count=4,
		time_delta=delta)
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
	tide_days = generate_tide_days(
		start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)),
		heights_count=2,
		time_delta=delta,
		start_life_cycle=TideHeight.LW)
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
	tide_days = generate_tide_days(
		start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)),
		heights_count=0,
		cycle_length=8, time_delta=delta)
	assert len(tide_days) == 8

	old_neap_level = tide_days[0].neap_level + 1
	for tide_day in tide_days:
		# check that neap levels are decreasing
		print(
			f"Neap level for day {tide_day.date.day} is {tide_day.neap_level}")
		assert tide_day.neap_level < old_neap_level
		old_neap_level = tide_day.neap_level

		# check that high water levels are increasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.HW,
						   predicate=lambda current, previous: current > previous + 0.04)

		# check that low water levels are decreasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.LW,
						   predicate=lambda current, previous: current < previous - 0.01)

	assert compute_max_hw(tide_days) == systest_get_hw(
		tide_days=tide_days, day_number=8, first=False)
	assert compute_max_lw(tide_days) == systest_get_lw(
		tide_days=tide_days, day_number=1, first=True)

	expected_springs_height = systest_get_mean_ht(tide_days=tide_days,
												  day_number=8)
	assert abs(compute_springs_mean(
		tide_days) - expected_springs_height) < 0.1

	expected_neaps_height = systest_get_mean_ht(tide_days=tide_days,
												day_number=1)
	assert abs(
		compute_neaps_mean(tide_days) - expected_neaps_height) < 0.1


def test_generate_one_cycle_various_water_height_factors():
	delta = datetime.timedelta(hours=6, minutes=20)
	common_params = dict(start_date=(
			reset_day() + datetime.timedelta(hours=3,
											 minutes=10)),
		heights_count=0, cycle_length=8,
		time_delta=delta)

	print(f"Tide days with min_water_factor=2, max_water_factor=5")
	tide_days = generate_tide_days(min_water_factor=2,
								   max_water_factor=5,
								   **common_params)
	[t.print() for t in tide_days]
	assert compute_max_hw(tide_days) > compute_max_lw(tide_days) + 1
	assert compute_springs_mean(tide_days) > compute_neaps_mean(
		tide_days) + 0.5
	print()

	print(f"Tide days with min_water_factor=3, max_water_factor=9")
	tide_days = generate_tide_days(min_water_factor=3,
								   max_water_factor=9,
								   **common_params)
	[t.print() for t in tide_days]
	assert compute_max_hw(tide_days) > compute_max_lw(tide_days) + 1
	assert compute_springs_mean(tide_days) > compute_neaps_mean(
		tide_days) + 0.5
	print()

	print(f"Tide days with min_water_factor=4, max_water_factor=11")
	tide_days = generate_tide_days(min_water_factor=4,
								   max_water_factor=11,
								   **common_params)
	[t.print() for t in tide_days]
	assert compute_max_hw(tide_days) > compute_max_lw(tide_days) + 1
	assert compute_springs_mean(tide_days) > compute_neaps_mean(
		tide_days) + 0.5
	print()


def test_generate_one_cycle_from_springs_to_neaps():
	delta = datetime.timedelta(hours=6, minutes=20)
	tide_days = generate_tide_days(
		start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)),
		heights_count=0,
		cycle_length=9, time_delta=delta,
		go_towards_springs=False)
	assert len(tide_days) == 9

	old_neap_level = tide_days[0].neap_level - 1
	for tide_day in tide_days:
		# check that neap levels are increasing
		print(
			f"Neap level for day {tide_day.date.day} is {tide_day.neap_level}")
		assert tide_day.neap_level > old_neap_level
		old_neap_level = tide_day.neap_level

		# check that high water levels are decreasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.HW,
						   predicate=lambda current, previous: current < previous - 0.04)

		# check that low water levels are increasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.LW,
						   predicate=lambda current, previous: current > previous + 0.01)

	assert compute_max_hw(tide_days) == systest_get_hw(
		tide_days=tide_days, day_number=1, first=True)
	assert compute_max_lw(tide_days) == systest_get_lw(
		tide_days=tide_days, day_number=9, first=False)

	expected_springs_height = systest_get_mean_ht(tide_days=tide_days, day_number=1)
	assert abs(compute_springs_mean(tide_days) - expected_springs_height) < 0.1

	expected_neaps_height = systest_get_mean_ht(tide_days=tide_days, day_number=9)
	assert abs(compute_neaps_mean(tide_days) - expected_neaps_height) < 0.1


def test_generate_range_increases_and_then_decreases():
	delta = datetime.timedelta(hours=6, minutes=20)
	tide_days = generate_tide_days(
		start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)),
		heights_count=0,
		cycle_length=9, time_delta=delta,
		go_towards_springs=True,
		start_days_after_neaps=4)
	assert len(tide_days) == 9

	first_neap_level = tide_days[0].neap_level

	# We start from the fifth day after neaps
	offset_neap_level = NEAP_MAX - 4 * (NEAP_MAX / 17)
	assert first_neap_level - offset_neap_level < 0.1

	# Tide height range should increase until reaching Springs
	old_neap_level = first_neap_level + 1
	for index in range(0, 5):
		tide_day = tide_days[index]

		# check that neap levels are decreasing
		print(
			f"Neap level for day {tide_day.date.day} is {tide_day.neap_level}")
		assert tide_day.neap_level < old_neap_level
		old_neap_level = tide_day.neap_level

		# check that high water levels are increasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.HW,
						   predicate=lambda current, previous: current > previous + 0.04)

		# check that low water levels are decreasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.LW,
						   predicate=lambda current, previous: current < previous - 0.01)

	# We have reached Springs
	middle_neap_level = tide_days[4].neap_level
	assert middle_neap_level == 0.0

	# From Springs, tide height range should decrease
	for index in range(5, len(tide_days)):
		tide_day = tide_days[index]

		# check that neap levels are increasing
		print(
			f"Neap level for day {tide_day.date.day} is {tide_day.neap_level}")
		assert tide_day.neap_level > old_neap_level
		old_neap_level = tide_day.neap_level

		# check that high water levels are decreasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.HW,
						   predicate=lambda current, previous: current < previous - 0.04)

		# check that low water levels are increasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.LW,
						   predicate=lambda current, previous: current > previous + 0.01)

	assert compute_max_hw(tide_days) == systest_get_hw(
		tide_days=tide_days, day_number=5, first=False)
	assert compute_max_lw(tide_days) == systest_get_lw(
		tide_days=tide_days, day_number=1, first=True)

	expected_springs_height = systest_get_mean_ht(tide_days=tide_days, day_number=5)
	assert abs(compute_springs_mean(tide_days) - expected_springs_height) < 0.1

	assert compute_neaps_mean(tide_days) is None


def test_generate_range_decreases_and_then_increases():
	delta = datetime.timedelta(hours=6, minutes=20)
	tide_days = generate_tide_days(
		start_date=(reset_day() + datetime.timedelta(hours=3, minutes=10)),
		heights_count=0,
		cycle_length=9, time_delta=delta,
		go_towards_springs=False,
		start_days_after_neaps=6)
	assert len(tide_days) == 9

	first_neap_level = tide_days[0].neap_level

	# We start from the seventh day after neaps
	offset_neap_level = NEAP_MAX - 6 * (NEAP_MAX / 17)
	assert first_neap_level - offset_neap_level < 0.1

	# Tide height range should decrease until reaching Neaps
	old_neap_level = first_neap_level - 1
	for index in range(0, 6):
		tide_day = tide_days[index]

		# check that neap levels are increasing
		print(
			f"Neap level for day {tide_day.date.day} is {tide_day.neap_level}")
		assert tide_day.neap_level > old_neap_level
		old_neap_level = tide_day.neap_level

		# check that high water levels are decreasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.HW,
						   predicate=lambda current, previous: current < previous - 0.04)

		# check that low water levels are increasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.LW,
						   predicate=lambda current, previous: current > previous + 0.01)

	# We have reached Neaps
	# Above and below, we skip the tests for day 7 (tide_days[6])
	# because tide changes right in the middle of the day and our
	# check_water_levels function is not prepared to handle that
	middle_neap_level = tide_days[6].neap_level
	assert middle_neap_level == NEAP_MAX

	# From Neaps, tide height range should increase
	for index in range(7, len(tide_days)):
		tide_day = tide_days[index]

		# check that neap levels are decreasing
		print(
			f"Neap level for day {tide_day.date.day} is {tide_day.neap_level}")
		assert tide_day.neap_level < old_neap_level
		old_neap_level = tide_day.neap_level

		# check that high water levels are increasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.HW,
						   predicate=lambda current, previous: current > previous + 0.04)

		# check that low water levels are decreasing
		check_water_levels(tide_day=tide_day,
						   tide_life_cycle=TideHeight.LW,
						   predicate=lambda current, previous: current < previous - 0.008)

	assert compute_max_hw(tide_days) == systest_get_hw(
		tide_days=tide_days, day_number=1, first=True)
	assert compute_max_lw(tide_days) == systest_get_lw(
		tide_days=tide_days, day_number=7, first=False)

	assert compute_springs_mean(tide_days) is None

	expected_neaps_height = systest_get_mean_ht(tide_days=tide_days, day_number=7)
	assert abs(compute_neaps_mean(tide_days) - expected_neaps_height) < 0.1
