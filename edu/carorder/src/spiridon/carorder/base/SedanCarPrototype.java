package spiridon.carorder.base;

import spiridon.carorder.caritems.AbsCarItem;
import spiridon.carorder.caritems.PassengerAirBagCarItem;
import spiridon.carorder.caritems.SedanChasisCarItem;

public class SedanCarPrototype extends CarPrototype {

	public SedanCarPrototype() {
		baseItems.add(new SedanChasisCarItem());
		baseItems.add(new AbsCarItem());
		baseItems.add(new PassengerAirBagCarItem());
	}
	
	@Override
	public String getName() {
		return "Mașină Berlină";
	}

}
