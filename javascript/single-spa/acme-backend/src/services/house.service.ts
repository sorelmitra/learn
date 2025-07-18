import { House, HouseListItem, HouseFeature } from '../dto/house';
import { getRawHouses } from '../database/house.repo';
import { NotFoundException } from '../exceptions';

export const getAllHouses = (): HouseListItem[] => {
	return getRawHouses().map(h => ({ id: h.id, address: h.address, price: h.price }));
};

export const getHouseById = (id: string): House => {
	const house = getRawHouses().find(h => h.id === id);
	if (!house) {
		throw new NotFoundException(`Could not find house with ID ${id}`);
	}
	return house;
};
