package spiridon.carmanage.masina;

import spiridon.carmanage.utils.Informatie;
import spiridon.carmanage.utils.InformatieSimpla;

public class StareMasinaDefecta implements StareMasina {

	@Override
	public Informatie informatii() {
		return new InformatieSimpla("Mașina este defectă");
	}

}
