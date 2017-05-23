package spiridon.carmanage.utils;

public class InformatieSimpla implements Informatie {

	private String info;

	public InformatieSimpla(String string) {
		info = string;
	}

	@Override
	public void afiseaza() {
		Logger.info(info);
	}

}
