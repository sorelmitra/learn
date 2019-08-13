package spiridon.carmanage.reparatii;

import spiridon.carmanage.executanti.Executant;
import spiridon.carmanage.masina.Masina;

public abstract class Reparatie {

	protected Executant executant;
	private Masina masina;

	public void executa() {
		detaliiReparatie();
		executant.repara(masina);
	}

	protected abstract void detaliiReparatie();

	public void setExecutant(Executant executant) {
		this.executant = executant;
	}

	public void setMasina(Masina m) {
		masina = m;
	}

}
