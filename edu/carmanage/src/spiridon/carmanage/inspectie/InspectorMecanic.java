package spiridon.carmanage.inspectie;

import spiridon.carmanage.executanti.ExecutantElectrician;
import spiridon.carmanage.executanti.ExecutantMecanic;
import spiridon.carmanage.executanti.ExecutantTinichigerie;
import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.reparatii.ListaReparatii;
import spiridon.carmanage.reparatii.Reparatie;
import spiridon.carmanage.reparatii.ReparatieAripa;
import spiridon.carmanage.reparatii.ReparatieBujii;
import spiridon.carmanage.reparatii.ReparatieFar;
import spiridon.carmanage.utils.Logger;

public class InspectorMecanic extends InspectorMasina {

	@Override
	public void umpleLista(ListaReparatii rep, Masina m) {
		Logger.titlu("Mecanicul inspectează");
		for (String problema : m.listaProbleme()) {
			if (problema.equalsIgnoreCase(Masina.PROBLEMA_BUJII)) {
				Logger.info("Adăugăm reparație bujii");
				Reparatie reparatie = new ReparatieBujii();
				reparatie.setMasina(m);
				reparatie.setExecutant(new ExecutantMecanic());
				rep.adauga(reparatie);
			}
		}
	}

}
