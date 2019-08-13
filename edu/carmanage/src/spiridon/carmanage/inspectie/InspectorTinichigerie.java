package spiridon.carmanage.inspectie;

import spiridon.carmanage.executanti.ExecutantElectrician;
import spiridon.carmanage.executanti.ExecutantTinichigerie;
import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.reparatii.ListaReparatii;
import spiridon.carmanage.reparatii.Reparatie;
import spiridon.carmanage.reparatii.ReparatieAripa;
import spiridon.carmanage.reparatii.ReparatieFar;
import spiridon.carmanage.utils.Logger;

public class InspectorTinichigerie extends InspectorMasina {

	@Override
	public void umpleLista(ListaReparatii rep, Masina m) {
		Logger.titlu("Tinichigerul inspectează");
		for (String problema : m.listaProbleme()) {
			if (problema.equalsIgnoreCase(Masina.PROBLEMA_ARIPA_INDOITA)) {
				Logger.info("Adăugăm reparație aripă");
				Reparatie reparatie = new ReparatieAripa();
				reparatie.setMasina(m);
				reparatie.setExecutant(new ExecutantTinichigerie());
				rep.adauga(reparatie);
			}
		}
	}

}
