package spiridon.carmanage.reparatii;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class ListaReparatii {

	private List<Reparatie> reparatii = new LinkedList<Reparatie>();
	private Iterator<Reparatie> iterator;
	
	public boolean executaUrmatoarea() {
		if (!iterator.hasNext()) {
			return false;
		}
		Reparatie reparatie = iterator.next();
		reparatie.executa();
		return true;
	}

	public void pregatesteReparatii() {
		iterator = reparatii.iterator();
	}

	public void adauga(Reparatie reparatie) {
		reparatii.add(reparatie);
	}

}
