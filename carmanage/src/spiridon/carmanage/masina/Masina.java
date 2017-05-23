package spiridon.carmanage.masina;

import java.util.LinkedList;
import java.util.List;

import spiridon.carmanage.utils.Informatie;

public class Masina {

	public static final String PROBLEMA_FAR_NEFUNCTIONAL = "Far nefuncțional";
	public static final String PROBLEMA_ARIPA_INDOITA = "Aripă îndoită";
	public static final String PROBLEMA_BUJII = "Bujie defectă";

	private List<String> listaProbleme = new LinkedList<String>();
	private StareMasina stare = new StareMasinaDefecta();
	
	public void setStare(StareMasina stare) {
		this.stare = stare;
	}

	public void adaugaProblema(String problema) {
		listaProbleme.add(problema);
	}

	public Informatie informatii() {
		return stare.informatii();
	}

	public List<String> listaProbleme() {
		return listaProbleme;
	}

}
