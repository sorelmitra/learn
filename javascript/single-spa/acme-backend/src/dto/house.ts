export type HouseAddress = {
	country: string;
	state: string;
	city: string;
	line1: string;
	line2?: string;
	postalCode?: string;
};

export type HouseBasicInfo = {
	id: string;
	address: HouseAddress;
	price: number;
};

export enum HouseFeature {
	AIR_CONDITIONING = 'AIR_CONDITIONING',
	BIG_BACKYARD = 'BIG_BACKYARD',
}

export type House = HouseBasicInfo & {
	picturePaths?: string[];
	features?: HouseFeature[];
};
