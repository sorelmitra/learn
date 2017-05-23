package spiridon.carmanage.executanti;

import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.masina.StareMasinaInLucru;
import spiridon.carmanage.utils.Logger;

public class ExecutantMecanic implements Executant {

	@Override
	public void repara(Masina m) {
		Logger.info("Mecanicul repară");
		m.setStare(new StareMasinaInLucru("3 zile"));
	}

}
