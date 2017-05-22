package spiridon.carmanage.reparatii;

import java.util.Iterator;
import java.util.List;

public class ListaReparatii {

	private List<Reparatie> reparatii;
	private Iterator<Reparatie> iterator;
	
	public boolean executaUrmatoarea() {
		if (!iterator.hasNext()) {
			return false;
		}
		Reparatie reparatie = iterator.next();
		return true;
	}

	public void pregatesteReparatii() {
		iterator = reparatii.iterator();
	}

}
