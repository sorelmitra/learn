package spiridon.carmanage.inspectie;

import spiridon.carmanage.executanti.ExecutantElectrician;
import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.reparatii.ListaReparatii;
import spiridon.carmanage.reparatii.Reparatie;
import spiridon.carmanage.reparatii.ReparatieFar;
import spiridon.carmanage.utils.Logger;

public class InspectorElectrician extends InspectorMasina {

	@Override
	public void umpleLista(ListaReparatii rep, Masina m) {
		Logger.titlu("Electricianul inspectează");
		for (String problema : m.listaProbleme()) {
			if (problema.equalsIgnoreCase(Masina.PROBLEMA_FAR_NEFUNCTIONAL)) {
				Logger.info("Adăugăm reparație far");
				Reparatie reparatie = new ReparatieFar();
				reparatie.setMasina(m);
				reparatie.setExecutant(new ExecutantElectrician());
				rep.adauga(reparatie);
			}
		}
	}

}
