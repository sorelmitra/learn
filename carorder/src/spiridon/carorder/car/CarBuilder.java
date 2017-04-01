package spiridon.carorder.car;

import spiridon.carorder.base.CarChassisNumberGenerator;
import spiridon.carorder.base.CarPrototype;
import spiridon.carorder.caritems.CarItem;
import spiridon.carorder.caritems.CarItemFactory;

public class CarBuilder {

	public Car createCar(CarPrototype prototype, String[] options, CarItemFactory carItemFactory, CarChassisNumberGenerator chassisNumberGenerator) throws Exception {
		Car car = new Car();
		car.setPrototype(prototype);
		for (String opt : options) {
			CarItem item = carItemFactory.createCarItem(opt);
			car.addOption(item);
		}
		car.setChassisNumber(chassisNumberGenerator.createChassisNumber());
		return car;
	}

}
