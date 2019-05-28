package com.yahoo.sorelmitra;

import java.util.Date;
import java.util.Random;

public class Main {

    private static Random random = new Random();

    public static void main(String[] args) {
        int nrOperatii = 40;
        random.setSeed(new Date().getTime());
        for (int i = 0; i < nrOperatii; i++) {
            w(i + 1 + ") ");
            sumaTrecerePesteOrdin();
            w("\n");
        }
    }

    private static void sumaTrecerePesteOrdin() {
        boolean trecerePesteOrdin[] = { false, true, true };
        StringBuffer s1 = new StringBuffer();
        StringBuffer s2 = new StringBuffer();
        for (int i = 0; i < trecerePesteOrdin.length; i++) {
            int cifra1;
            if (trecerePesteOrdin[i]) {
                cifra1 = random.nextInt(9) + 1;
            } else {
                cifra1 = random.nextInt(7) + 1;
            }
            //w("cifra1 " + cifra1);
            int cifra2;
            if (trecerePesteOrdin[i]) {
                int diff = 10 - cifra1;
                cifra2 = random.nextInt(cifra1 + 1) + diff - 1;
            } else {
                int diff = 9 - cifra1;
                cifra2 = random.nextInt(diff - 1) + 1;
            }
            //w("cifra2 " + cifra2);
            s1.append(cifra1);
            s2.append(cifra2);
        }
        int nr1 = Integer.valueOf(s1.toString()).intValue();
        int nr2 = Integer.valueOf(s2.toString()).intValue();
        int suma = nr1 + nr2;
        w(nr1 + " + " + nr2 + " = " + suma);
    }

    private static void w(String s) {
        System.out.print(s);
    }
}
