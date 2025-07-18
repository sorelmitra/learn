import { House, HouseFeature } from '../dto/house';

export const getRawHouses = (): House[] => {
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
