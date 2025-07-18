export type HouseAddress = {
	country: string;
	state: string;
	city: string;
	line1: string;
	line2?: string;
	postalCode?: string;
};

export type HouseListItem = {
	id: string;
	address: HouseAddress;
	price: number;
};

export enum HouseFeature {
	AIR_CONDITIONING = 'AIR_CONDITIONING',
	BIG_BACKYARD = 'BIG_BACKYARD',
}

export type House = HouseListItem & {
	picturePaths?: string[];
	features?: HouseFeature[];
};
