package spiridon.carorder.base;

import java.math.BigInteger;
import java.security.SecureRandom;

public class CarChassisNumberGenerator {

	private static CarChassisNumberGenerator instance = null;

	private SecureRandom random = new SecureRandom();

	private CarChassisNumberGenerator() {
	}
	
	public static CarChassisNumberGenerator getInstance() {
		if (instance != null) {
			return instance;
		}
		instance = new CarChassisNumberGenerator();
		return instance;
	}

	public String createChassisNumber() {
		return new BigInteger(130, random).toString(32).toUpperCase();
	}
}
