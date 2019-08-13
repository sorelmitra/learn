package spiridon.carmanage.executanti;

import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.masina.StareMasinaInLucru;
import spiridon.carmanage.utils.Logger;

public class ExecutantTinichigerie implements Executant {

	@Override
	public void repara(Masina m) {
		Logger.info("Tinichigerul repară");
		m.setStare(new StareMasinaInLucru("4 zile"));
	}

}
