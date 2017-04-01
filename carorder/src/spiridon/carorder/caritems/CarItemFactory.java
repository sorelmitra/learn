package spiridon.carorder.caritems;

public class CarItemFactory {

	public CarItem createCarItem(String opt) throws Exception {
		switch (opt) {
		case CarOptions.REAR_WIPER: return new RearWiperCarItem(); 
		case CarOptions.SPORT_SEATS: return new SportSeatsCarItem(); 
		case CarOptions.RAIN_SENSOR: return new RainSensorCarItem(); 
		case CarOptions.XENON: return new XenonCarItem(); 
		case CarOptions.LEATHER_SEATS: return new LeatherSeatsCarItem(); 
		case CarOptions.AIR_CONDITIONING: return new AirConditioningCarItem();
		case CarOptions.ELECTRIC_MIRRORS: return new ElectricMirrorsCarItem();
		case CarOptions.AUTOMATIC_GEARBOX: return new AutomaticGearboxCarItem();
		case CarOptions.ALLOY_RIMS: return new AlloyRimsCarItem();
		case CarOptions.RADIO: return new RadioCarItem();
		case CarOptions.NAVIGATION: return new NavigationCarItem();
		case CarOptions.PARKING_SENSORS: return new ParkingSensorsCarItem();
		case CarOptions.METALLIC_PAINT: return new MetallicPaintCarItem();
		default: throw new Exception("Unknown option " + opt);
		}
	}

}
