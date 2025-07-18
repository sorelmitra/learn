import { House, HouseListItem, HouseFeature } from '../dto/house';
import { NotFoundException } from '../exceptions';

const getRawHouses = (): House[] => {
	return [
		{
			id: '1',
			address: {
				country: 'USA',
				state: 'New York',
				city: 'New York City',
				line1: '123 Rue Avenue',
				postalCode: '10029',
			},
			price: 230000,
			features: [HouseFeature.AIR_CONDITIONING],
			picturePaths: ['img/house1.jpg'],
		},
		{
			id: '2',
			address: {
				country: 'Australia',
				state: 'New South Wales',
				city: 'Sydney',
				line1: '456 Big Boulevard',
			},
			price: 1800500,
			features: [HouseFeature.BIG_BACKYARD],
			picturePaths: ['img/house1.jpg'],
		},
	];
};

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
