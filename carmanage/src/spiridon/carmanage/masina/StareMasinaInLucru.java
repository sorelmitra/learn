package spiridon.carmanage.masina;

import spiridon.carmanage.utils.Informatie;
import spiridon.carmanage.utils.InformatieSimpla;

public class StareMasinaInLucru implements StareMasina {

	private String durata;

	public StareMasinaInLucru(String durata) {
		this.durata = durata;
	}

	@Override
	public Informatie informatii() {
		return new InformatieSimpla("Mașina este în lucru, mai durează " + durata);
	}

}
