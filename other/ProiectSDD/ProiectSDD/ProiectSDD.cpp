// ProiectSDD.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <string.h>
#include <WinSock2.h>
#include <time.h>
#include <sys/timeb.h>

enum CAUTARI_SIR {
	NUME_COD = 10,
	ADRESA_IP,
};

enum CAUTARI_NR {
	NR_PAGINI = 20,
	NR_DOWNLOAD,
	CANT_UPLOAD,
};

struct DateAplicatie {
	unsigned int nrPagini;
	unsigned int nrAccesari;
	unsigned int nrClickuri;
};

struct DateTrafic {
	unsigned int nr;
	unsigned int cant;
};

struct Utilizator {
	char numeCod[30];
	char adresaIP[16];
	char ultimAcces[26];
	DateAplicatie apl;
	DateTrafic dwn;
	DateTrafic up;
};

struct ListaUtilizatori {
	unsigned int lungime;
	Utilizator *date;
};

ListaUtilizatori utilizatori;

void afisareApl(DateAplicatie *apl)
{
	printf("- Accesari %u, Pagini %u, Clickuri %u\n",
		apl->nrAccesari, apl->nrPagini, apl->nrClickuri);
}

void afisareTrafic(DateTrafic *t)
{
	printf("- Download: nr. total %u, cant %u Mb\n",
		t->nr, t->cant);
}

void afisareUtilizator(Utilizator *u, BOOL detalii = TRUE)
{
	printf("Utilizator '%s', IP %s, ultima accessare %s\n", 
		u->numeCod, u->adresaIP, u->ultimAcces);
	if (detalii) {
		afisareApl(&(u->apl));
		afisareTrafic(&(u->dwn));
		afisareTrafic(&(u->up));
	}
}

void meniuIndice()
{
	unsigned int indice;
	printf("Introduceti indicele\n> ");
	scanf_s("%u", &indice);
	if (indice >= utilizatori.lungime) {
		printf("Indice gresit, maxim %u\n", utilizatori.lungime);
		return;
	}
	afisareUtilizator(&(utilizatori.date[indice]));
}

Utilizator *cautareSir(CAUTARI_SIR ce_caut, char *s)
{
	unsigned int i;
	for (i = 0; i < utilizatori.lungime; i++) {
		switch (ce_caut) {
		case NUME_COD:
			if (lstrcmpiA(utilizatori.date[i].numeCod, s) == 0) {
				return &(utilizatori.date[i]);
			}
			break;
		case ADRESA_IP:
			if (lstrcmpiA(utilizatori.date[i].adresaIP, s) == 0) {
				return &(utilizatori.date[i]);
			}
			break;
		}
	}

	return NULL;
}

void meniuNumeCod()
{
	char s[128];
	printf("Introduceti nume cod\n> ");
	scanf_s("%s", s, sizeof(s));
	Utilizator *u = cautareSir(NUME_COD, s);
	if (u == NULL) {
		printf("Nu exista utilizatori cu numele de cod '%s'\n", s);
		return;
	}
	afisareUtilizator(u);
}

void meniuAdresaIP()
{
	char s[16];
	printf("Introduceti adresa IP\n> ");
	scanf_s("%s", s, sizeof(s));
	Utilizator *u = cautareSir(ADRESA_IP, s);
	if (u == NULL) {
		printf("Nu exista utilizatori cu adresa IP '%s'\n", s);
		return;
	}
	afisareUtilizator(u);
}

void meniuUtilizator()
{
	unsigned int c = 0;
	while (c != 4) {
		printf("1. Dupa indice\n");
		printf("2. Dupa nume cod\n");
		printf("3. Dupa adresa IP\n");
		printf("4. Revenire");
		printf("> ");
		scanf_s("%u", &c);
		switch (c) {
		case 1: meniuIndice(); break;
		case 2: meniuNumeCod(); break;
		case 3: meniuAdresaIP(); break;
		}
	}
}

int cautareNr(int start, CAUTARI_NR ce_caut, unsigned int n)
{
	int i;
	for (i = start + 1; (unsigned int) i < utilizatori.lungime; i++) {
		switch (ce_caut) {
		case NR_PAGINI:
			if (utilizatori.date[i].apl.nrPagini == n) {
				return i;
			}
			break;
		case NR_DOWNLOAD:
			if (utilizatori.date[i].dwn.nr == n) {
				return i;
			}
			break;
		case CANT_UPLOAD:
			if (utilizatori.date[i].up.cant == n) {
				return i;
			}
			break;
		}
	}

	return -1;
}

void meniuNr(CAUTARI_NR ce_caut, char *ce_afisez)
{
	unsigned int n;
	printf("Introduceti %s\n> ", ce_afisez);
	scanf_s("%u", &n);
	int i = cautareNr(-1, ce_caut, n);
	if (i == -1) {
		printf("Nu exista utilizatori care au accesat %u %s\n", n, ce_afisez);
		return;
	}
	printf("Au accesat %u %s:\n", n, ce_afisez);
	for (; i != -1; i = cautareNr(i, ce_caut, n)) {
		afisareUtilizator(&(utilizatori.date[i]), FALSE);
	}
}

void meniuFiltre()
{
	unsigned int c = 0;
	while (c != 4) {
		printf("1. Dupa nr. pagini\n");
		printf("2. Dupa nr. download\n");
		printf("3. Dupa cant. upload\n");
		printf("4. Revenire");
		printf("> ");
		scanf_s("%u", &c);
		switch (c) {
		case 1: meniuNr(NR_PAGINI, "pagini"); break;
		case 2: meniuNr(NR_DOWNLOAD, "download-uri"); break;
		case 3: meniuNr(CANT_UPLOAD, "Mb upload"); break;
		}
	}
}

int main()
{
	utilizatori.lungime = 3;
	utilizatori.date = (Utilizator *)malloc(utilizatori.lungime * sizeof(Utilizator));
	strcpy_s(utilizatori.date[0].numeCod, 30, "gigi");
	strcpy_s(utilizatori.date[0].adresaIP, 16, "10.1.3.4");
	strcpy_s(utilizatori.date[0].ultimAcces, 26, "Apr 25");
	utilizatori.date[0].apl.nrAccesari = 3;
	utilizatori.date[0].apl.nrPagini = 10;
	utilizatori.date[0].apl.nrClickuri = 13;
	utilizatori.date[0].dwn.nr = 3;
	utilizatori.date[0].dwn.cant = 400;
	utilizatori.date[0].up.nr = 4;
	utilizatori.date[0].up.cant = 100;
	strcpy_s(utilizatori.date[1].numeCod, 30, "dorel");
	strcpy_s(utilizatori.date[1].adresaIP, 16, "10.1.3.5");
	strcpy_s(utilizatori.date[1].ultimAcces, 26, "May 5");
	utilizatori.date[1].apl.nrAccesari = 5;
	utilizatori.date[1].apl.nrPagini = 16;
	utilizatori.date[1].apl.nrClickuri = 18;
	utilizatori.date[1].dwn.nr = 2;
	utilizatori.date[1].dwn.cant = 450;
	utilizatori.date[1].up.nr = 10;
	utilizatori.date[1].up.cant = 200;
	strcpy_s(utilizatori.date[2].numeCod, 30, "titel");
	strcpy_s(utilizatori.date[2].adresaIP, 16, "10.1.3.6");
	strcpy_s(utilizatori.date[2].ultimAcces, 26, "Jun 20");
	utilizatori.date[2].apl.nrAccesari = 3;
	utilizatori.date[2].apl.nrPagini = 10;
	utilizatori.date[2].apl.nrClickuri = 13;
	utilizatori.date[2].dwn.nr = 3;
	utilizatori.date[2].dwn.cant = 300;
	utilizatori.date[2].up.nr = 1;
	utilizatori.date[2].up.cant = 10;
	unsigned int c = 0;
	while (c != 3) {
		printf("1. Afisare date utilizator\n");
		printf("2. Filtre\n");
		printf("3. Iesire\n");
		printf("> ");
		scanf_s("%u", &c);
		switch (c) {
		case 1: meniuUtilizator(); break;
		case 2: meniuFiltre(); break;
		}
	}
    return 0;
}

