package spiridon.carmanage.main;

import spiridon.carmanage.inspectie.InspectorElectrician;
import spiridon.carmanage.inspectie.InspectorMasina;
import spiridon.carmanage.inspectie.InspectorMecanic;
import spiridon.carmanage.inspectie.InspectorTinichigerie;
import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.masina.StareMasinaGata;
import spiridon.carmanage.masina.StareMasinaInLucru;
import spiridon.carmanage.reparatii.ListaReparatii;
import spiridon.carmanage.utils.Informatie;
import spiridon.carmanage.utils.Logger;

public class CarManage {

	public static void main(String[] args) {
		Masina m = new Masina();
		m.adaugaProblema(Masina.PROBLEMA_FAR_NEFUNCTIONAL);
		m.adaugaProblema(Masina.PROBLEMA_ARIPA_INDOITA);
		
		Logger.sectiune("La venirea în service");
		Informatie info = m.informatii();
		info.afiseaza();
		
		Logger.sectiune("Se inspectează");
		ListaReparatii reparatii = inspecteaza(m);
		reparatii.pregatesteReparatii();
		
		Logger.sectiune("Se execută reparațiile");
		while (reparatii.executaUrmatoarea()) {
			info = m.informatii();
			info.afiseaza();
		}
		Logger.sectiune("S-au terminat reparațiile");
		m.setStare(new StareMasinaGata("peste 1 zi"));
		info = m.informatii();
		info.afiseaza();
	}

	private static ListaReparatii inspecteaza(Masina m) {
		InspectorMasina elec = new InspectorElectrician();
		InspectorMasina mec = new InspectorMecanic();
		InspectorMasina tini = new InspectorTinichigerie();
		tini.puneUrmatorul(mec);
		mec.puneUrmatorul(elec);
		ListaReparatii reparatii = new ListaReparatii();
		tini.inspecteaza(m, reparatii);
		return reparatii;
	}

}
