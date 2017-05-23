package spiridon.carmanage.utils;

public class Logger {

	public static void info(String string) {
		System.out.println(string);
	}

	public static void sectiune(String string) {
		info("\n=======================================");
		info(string);
		info("=======================================\n");
	}

	public static void titlu(String string) {
		info("\n" + string);
	}

}
