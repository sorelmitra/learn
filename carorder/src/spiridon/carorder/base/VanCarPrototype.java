package spiridon.carorder.base;

import spiridon.carorder.caritems.AbsCarItem;
import spiridon.carorder.caritems.PassengerAirBagCarItem;
import spiridon.carorder.caritems.VanChasisCarItem;

public class VanCarPrototype extends CarPrototype {

	public VanCarPrototype() {
		baseItems.add(new VanChasisCarItem());
		baseItems.add(new AbsCarItem());
		baseItems.add(new PassengerAirBagCarItem());
	}
	
	@Override
	public String getName() {
		return "Mașină Dubiță";
	}

}
