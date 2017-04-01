package spiridon.carorder.base;

import spiridon.carorder.caritems.AbsCarItem;
import spiridon.carorder.caritems.PassengerAirBagCarItem;
import spiridon.carorder.caritems.RearWiperCarItem;
import spiridon.carorder.caritems.WagonChasisCarItem;

public class WagonCarPrototype extends CarPrototype {

	public WagonCarPrototype() {
		baseItems.add(new WagonChasisCarItem());
		baseItems.add(new AbsCarItem());
		baseItems.add(new PassengerAirBagCarItem());
		baseItems.add(new RearWiperCarItem());
	}
	
	@Override
	public String getName() {
		return "Mașină Break";
	}

}
