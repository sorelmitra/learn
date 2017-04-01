package spiridon.carorder.main;

import java.io.IOException;
import java.io.StringWriter;

import spiridon.carorder.base.CarChassisNumberGenerator;
import spiridon.carorder.base.CarPrototype;
import spiridon.carorder.base.CarPrototypeCache;
import spiridon.carorder.car.Car;
import spiridon.carorder.car.CarBuilder;
import spiridon.carorder.caritems.CarItemFactory;
import spiridon.carorder.caritems.CarOptions;

public class CarOrder {

	public static void main(String[] args) throws Exception {
		StringWriter w = new StringWriter();

		CarPrototypeCache prototypeCache = new CarPrototypeCache();
		
		CarPrototype prototype = null;		
		Car car = null;

		CarBuilder carBuilder = new CarBuilder();
		CarItemFactory carItemFactory = new CarItemFactory();
		
		prototype = prototypeCache.getPrototypeInstance("sedan");
		car = carBuilder.createCar(prototype, new String[] {CarOptions.XENON, CarOptions.RAIN_SENSOR}, carItemFactory, CarChassisNumberGenerator.getInstance());
		car.show(w);
		
		prototype = prototypeCache.getPrototypeInstance("coupe");
		car = carBuilder.createCar(prototype, new String[] {CarOptions.ELECTRIC_MIRRORS, CarOptions.RADIO, CarOptions.LEATHER_SEATS, CarOptions.AUTOMATIC_GEARBOX}, carItemFactory, CarChassisNumberGenerator.getInstance());
		car.show(w);
		
		prototype = prototypeCache.getPrototypeInstance("wagon");
		car = carBuilder.createCar(prototype, new String[] {CarOptions.METALLIC_PAINT, CarOptions.NAVIGATION}, carItemFactory, CarChassisNumberGenerator.getInstance());
		car.show(w);
		
		prototype = prototypeCache.getPrototypeInstance("van");
		car = carBuilder.createCar(prototype, new String[] {CarOptions.RADIO, CarOptions.PARKING_SENSORS}, carItemFactory, CarChassisNumberGenerator.getInstance());
		car.show(w);
		
		System.out.println(w.toString());
	}

}
