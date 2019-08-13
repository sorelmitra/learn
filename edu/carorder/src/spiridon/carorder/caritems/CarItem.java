package spiridon.carorder.caritems;

import java.io.IOException;
import java.io.Writer;

public abstract class CarItem {
	public abstract double getPrice();
	
	public abstract String getName();

	public void show(Writer w) throws IOException {
		w.write(getName() + ": " + Double.toString(getPrice()) + " EUR\n");
	}
}
