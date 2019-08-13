package spiridon.carorder.base;

import java.util.Hashtable;

public class CarPrototypeCache {

	private Hashtable<String, CarPrototype> prototypeMap;

	public CarPrototypeCache() {
		prototypeMap = new Hashtable<>();
		prototypeMap.put("sedan", new SedanCarPrototype());
		prototypeMap.put("coupe", new CoupeCarPrototype());
		prototypeMap.put("wagon", new WagonCarPrototype());
		prototypeMap.put("van", new VanCarPrototype());
	}
	
	public CarPrototype getPrototypeInstance(String id) throws CloneNotSupportedException {
		CarPrototype prototype = prototypeMap.get(id);
		return (CarPrototype)prototype.clone();
	}

}
