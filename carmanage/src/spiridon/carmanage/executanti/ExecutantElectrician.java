package spiridon.carmanage.executanti;

import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.masina.StareMasinaInLucru;
import spiridon.carmanage.utils.Logger;

public class ExecutantElectrician implements Executant {

	@Override
	public void repara(Masina m) {
		Logger.info("Electricianul repară");
		m.setStare(new StareMasinaInLucru("5 zile"));
	}

}
