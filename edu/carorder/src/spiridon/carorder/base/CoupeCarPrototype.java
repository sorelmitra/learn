package spiridon.carorder.base;

import spiridon.carorder.caritems.AbsCarItem;
import spiridon.carorder.caritems.CoupeChasisCarItem;
import spiridon.carorder.caritems.PassengerAirBagCarItem;
import spiridon.carorder.caritems.SportSeatsCarItem;

public class CoupeCarPrototype extends CarPrototype {

	public CoupeCarPrototype() {
		baseItems.add(new CoupeChasisCarItem());
		baseItems.add(new AbsCarItem());
		baseItems.add(new PassengerAirBagCarItem());
		baseItems.add(new SportSeatsCarItem());
	}
	
	@Override
	public String getName() {
		return "Mașină Coupe";
	}

}
