package spiridon.carmanage.main;

import spiridon.carmanage.reparatii.ListaReparatii;
import spiridon.carmanage.utils.Logger;

public class CarManage {

	public static void main(String[] args) {
		Masina m = new Masina();
		m.adaugaProblema(PROBLEMA_FAR_NEFUNCTIONAL);
		m.adaugaProblema(PROBLEMA_ARIPA_INDOITA);
		
		ListaReparatii reparatii = inspecteaza(m);
		reparatii.pregatesteReparatii();
		
		if (!reparatii.executaUrmatoarea()) {
			Logger.info("Nu sunt reparații de executat");
			return;
		}
		
		Informatie info = m.informatii();
		info.afiseaza();
		
		if (!reparatii.executaUrmatoarea()) {
			Logger.info("S-au terminat reparațiile");
		}
		info = m.informatii();
		info.afiseaza();
	}

	private static ListaReparatii inspecteaza(Masina m) {
		InspectorMasina elec = new InspectorElectrician();
		InspectorMasina mec = new InspectorMecanic(elec);
		InspectorMasina tini = new InspectorTinichigerie(mec);
		ListaReparatii reparatii = tini.inspecteaza(m);
		return reparatii;
	}

}
