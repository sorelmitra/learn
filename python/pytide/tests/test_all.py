from pytide import semidiurnal_tide


def truncate(value):
	return int(value * 10) / 10.0


def test_heights():
	tide_factors = dict(min_water_factor=2, max_water_factor=5)
	neap_level = 5
	compute_springs_height = semidiurnal_tide(**tide_factors)
	compute_neaps_height = semidiurnal_tide(**tide_factors, neap_factor=neap_level)

	print('Neaps for this tide cycle')
	neaps_lw = compute_neaps_height(0)
	neaps_hw = compute_neaps_height(6)
	neaps_lw2 = compute_neaps_height(12)
	print(f"LW={format(neaps_lw, '.1f')}")
	print(f"HW={format(neaps_hw, '.1f')}")
	print(f"LW={format(neaps_lw2, '.1f')}")
	print('All neaps hours')
	neap_heights = []
	old_ht = 0
	for i2 in range(0, 13):
		ht = compute_neaps_height(i2)
		if i2 <= 6:
			assert old_ht < ht
		else:
			assert ht < old_ht
		old_ht = ht
		neap_heights.append(ht)
		print(f"{i2}h={format(ht, '.1f')}")
	print()
	assert neap_heights[0] == neaps_lw
	assert neap_heights[6] == neaps_hw
	assert neap_heights[12] == neaps_lw2
	assert truncate(neaps_lw) == truncate(neaps_lw2)

	print('Springs for this tide cycle')
	springs_lw = compute_springs_height(0)
	springs_hw = compute_springs_height(6)
	springs_lw2 = compute_springs_height(12)
	print(f"LW={format(springs_lw, '.1f')}")
	print(f"HW={format(springs_hw, '.1f')}")
	print(f"LW={format(springs_lw2, '.1f')}")
	print('All springs hours')
	spring_heights = []
	old_ht = 0
	for i3 in range(0, 13):
		ht = compute_springs_height(i3)
		if i3 <= 6:
			assert old_ht < ht
		else:
			assert ht < old_ht
		old_ht = ht
		spring_heights.append(ht)
		print(f"{i3}h={format(ht, '.1f')}")
	print()
	assert spring_heights[0] == springs_lw
	assert spring_heights[6] == springs_hw
	assert spring_heights[12] == springs_lw2
	assert truncate(springs_lw) == truncate(springs_lw2)

	assert springs_lw < neaps_lw
	assert neaps_lw < neaps_hw
	assert neaps_hw < springs_hw

