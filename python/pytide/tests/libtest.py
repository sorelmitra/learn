from src.tide_tables import TideHeight


def systest_get_hw(*, tide_days, day_number, first):
	index = day_number - 1
	high_waters = [t for t in tide_days[index].heights if t.type == TideHeight.HW]
	if first:
		return high_waters[0].height
	return high_waters[len(high_waters) - 1].height


def systest_get_lw(*, tide_days, day_number, first):
	index = day_number - 1
	low_waters = [t for t in tide_days[index].heights if t.type == TideHeight.LW]
	if first:
		return low_waters[0].height
	return low_waters[len(low_waters) - 1].height


def systest_get_first_ht(*, tide_days, day_number):
	index = day_number - 1
	hw = [t for t in tide_days[index].heights if t.type == TideHeight.HW][0]
	lw = [t for t in tide_days[index].heights if t.type == TideHeight.LW][0]
	return hw.height - lw.height


def systest_get_last_ht(*, tide_days, day_number):
	index = day_number - 1
	high_waters = [t for t in tide_days[index].heights if t.type == TideHeight.HW]
	hw = high_waters[len(high_waters) - 1]
	low_waters = [t for t in tide_days[index].heights if t.type == TideHeight.LW]
	lw = low_waters[len(low_waters) - 1]
	return hw.height - lw.height


def systest_get_mean_ht(*, tide_days, day_number):
	first_springs_height = systest_get_first_ht(tide_days=tide_days, day_number=day_number)
	last_springs_height = systest_get_last_ht(tide_days=tide_days, day_number=day_number)
	return (first_springs_height + last_springs_height) / 2


def check_water_levels(
		*, tide_day, tide_life_cycle,
		predicate=lambda current_ht, previous_ht: current_ht == previous_ht):
	prev_tide_value = None
	for tide_value in tide_day.heights:
		if tide_value.type == tide_life_cycle:
			if prev_tide_value is None:
				prev_tide_value = tide_value
			else:
				print(
					f"{tide_life_cycle} height for day {tide_day.date.day}, time {tide_value.time.strftime('%H%M')} is {format(tide_value.height, '.2f')}, previous was time {prev_tide_value.time.strftime('%H%M')} {format(prev_tide_value.height, '.2f')}")
				assert predicate(tide_value.height,
								 prev_tide_value.height)
				prev_tide_value = tide_value
	print()
