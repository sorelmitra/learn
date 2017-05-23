package spiridon.carmanage.masina;

import spiridon.carmanage.utils.Informatie;
import spiridon.carmanage.utils.InformatieSimpla;

public class StareMasinaGata implements StareMasina {

	private String ridicare;

	public StareMasinaGata(String s) {
		this.ridicare = s;
	}

	@Override
	public Informatie informatii() {
		return new InformatieSimpla("Mașina este gata, se poate ridica " + ridicare);
	}

}
