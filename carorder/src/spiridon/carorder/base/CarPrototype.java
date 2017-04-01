package spiridon.carorder.base;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;

import spiridon.carorder.caritems.CarItem;

public abstract class CarPrototype implements Cloneable {

	protected ArrayList<CarItem> baseItems;

	public CarPrototype() {
		baseItems = new ArrayList<CarItem>();
	}
	
	public void show(Writer w) throws IOException {
		w.write("\n\n" + getName() + "\n");
		w.write("==============\n\n");
		w.write("Elemente de bază:\n");
		w.write("-----------------\n");
		for (CarItem carItem : baseItems) {
			carItem.show(w);
		}
		w.write("-----------------\n");
		w.write("Preț de bază: " + Double.toString(getPrice()) + " EUR\n");
	}

	public abstract String getName();
	
	public double getPrice() {
		double price = 0;
		for (CarItem carItem : baseItems) {
			price += carItem.getPrice();
		}
		return price;
	}

	@Override
	protected Object clone() throws CloneNotSupportedException {
		return super.clone();
	}
}
