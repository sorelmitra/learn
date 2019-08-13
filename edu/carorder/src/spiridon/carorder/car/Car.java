package spiridon.carorder.car;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;

import spiridon.carorder.base.CarPrototype;
import spiridon.carorder.caritems.CarItem;

public class Car {

	private CarPrototype prototype;
	private ArrayList<CarItem> carItems;
	private String chassisNumber;

	public Car() {
		carItems = new ArrayList<CarItem>();
	}
	
	public void setChassisNumber(String chassisNumber) {
		this.chassisNumber = chassisNumber;
	}

	private String getChassisNumber() {
		return chassisNumber;
	}

	public void setPrototype(CarPrototype prototype) {
		this.prototype = prototype;
	}

	public void addOption(CarItem item) {
		carItems.add(item);
	}

	public void show(StringWriter w) throws IOException {
		w.write("\n\n************************\n");
		w.write("*** " + getName() + "\n");
		w.write("************************");
		prototype.show(w);
		w.write("\nOpțiuni:\n");
		w.write("--------\n");
		for (CarItem carItem : carItems) {
			carItem.show(w);
		}
		w.write("--------\n");
		w.write("Preț opțiuni: " + Double.toString(getPrice()) + " EUR\n\n");
		w.write("Serie șasiu: " + getChassisNumber() + "\n\n");
		w.write("************************\n");
		w.write("Preț TOTAL " + prototype.getName() + ": " + Double.toString(prototype.getPrice() + getPrice()) + " EUR\n");
	}

	private String getName() {
		return "Mașina aleasă";
	}

	protected double getPrice() {
		double price = 0;
		for (CarItem carItem : carItems) {
			price += carItem.getPrice();
		}
		return price;
	}

}
