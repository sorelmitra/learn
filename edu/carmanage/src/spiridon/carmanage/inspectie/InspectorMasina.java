package spiridon.carmanage.inspectie;

import spiridon.carmanage.masina.Masina;
import spiridon.carmanage.reparatii.ListaReparatii;

public abstract class InspectorMasina {

	protected InspectorMasina urmatorul;
	
	public ListaReparatii inspecteaza(Masina m, ListaReparatii listaReparatii) {
		umpleLista(listaReparatii, m);
		if (urmatorul != null) {
			return urmatorul.inspecteaza(m, listaReparatii);
		}
		return listaReparatii;
	}

	public abstract void umpleLista(ListaReparatii rep, Masina m);

	public void puneUrmatorul(InspectorMasina urmatorul) {
		this.urmatorul = urmatorul;
	}

}
