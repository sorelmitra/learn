#include "stdafx.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

///////////////////////////////////////////////////////////
// lista - Trafic date
///////////////////////////////////////////////////////////

struct trafic_date {
	int index_utiliz;
	int upload;
	int download;
};

struct nod_lista
{
	struct trafic_date trafic;
	struct nod_lista *next;
};

struct nod_lista *lista_cap;

void lista_inserare(struct trafic_date trafic)
{
	struct nod_lista *temp;
	temp = (struct nod_lista *)malloc(sizeof(struct nod_lista));
	temp->trafic = trafic;
	if (lista_cap == NULL)
	{
		lista_cap = temp;
		lista_cap->next = NULL;
	}
	else
	{
		temp->next = lista_cap;
		lista_cap = temp;
	}
}

void lista_adauga(struct trafic_date trafic)
{
	if (lista_cap == NULL) {
		lista_inserare(trafic);
		return;
	}

	struct nod_lista *temp, *p;
	temp = (struct nod_lista *)malloc(sizeof(struct nod_lista));
	temp->trafic = trafic;
	p = (struct nod_lista *)lista_cap;
	while (p->next != NULL)
		p = p->next;
	p->next = temp;
	p = temp;
	p->next = NULL;
}

int lista_sterge(struct trafic_date trafic)
{
	struct nod_lista *temp, *prev;
	temp = lista_cap;
	prev = NULL;
	while (temp != NULL)
	{
		if (temp->trafic.index_utiliz == trafic.index_utiliz)
		{
			if (temp == lista_cap)
			{
				lista_cap = temp->next;
			}
			else
			{
				prev->next = temp->next;
			}
			free(temp);
			return 1;
		}
		else
		{
			prev = temp;
			temp = temp->next;
		}
	}
	return 0;
}

int lista_nr_elem()
{
	struct nod_lista *n;
	int c = 0;
	n = lista_cap;
	while (n != NULL)
	{
		n = n->next;
		c++;
	}
	return c;
}

void lista_afis()
{
	struct nod_lista *r = lista_cap;
	if (r == NULL)
	{
		return;
	}
	// conversie lista in vector
	int n = lista_nr_elem();
	struct trafic_date *trafice = (struct trafic_date *)malloc(sizeof(struct trafic_date) * n);
	int i = 0;
	while (r != NULL)
	{
		trafice[i++] = r->trafic;
		r = r->next;
	}
	// afisare vector
	printf("Trafic date utilizatori {Mb upload, Mb download}\n");
	for (i = 0; i < n; i++) {
		printf("utiliz %d {%d, %d}\n", trafice[i].index_utiliz, trafice[i].upload, trafice[i].download);
	}
	printf("\n");
	free(trafice);
}

void lista_citire(char *fname)
{
	FILE *f = NULL;
	if (0 != fopen_s(&f, fname, "rt")) {
		printf("Nu pot deschide fisierul %s\n", fname);
		return;
	}
	char buf[32];
	struct trafic_date trafic;
	while (fgets(buf, 31, f)) {
		sscanf_s(buf, "%d: %d, %d", &(trafic.index_utiliz), &(trafic.upload), &(trafic.download));
		lista_adauga(trafic);
	}
	fclose(f);
}


///////////////////////////////////////////////////////////
// matrice - Pagini deschise per utilizator
///////////////////////////////////////////////////////////

int nr_pagini = 0;
int nr_utiliz = 0;
// Pe randuri, utilizatorii - fiecare rand un utilizator
// Pe coloane, paginile - fiecare coloana reprezinta o pagina din aplicatia web
int **pagini_per_utiliz;

void matr_citire(char *fname)
{
	FILE *f = NULL;
	if (0 != fopen_s(&f, fname, "rt")) {
		printf("Nu pot deschide fisierul %s\n", fname);
		return;
	}
	char buf[32];
	// fisierul are formatul: prima linie - nr pagini; a doua linie - nr utilizatori; liniile urmatoare: de cate ori s-a accesat pagina curenta. Fiecare linie corespunde unei pagini. O linie goala separa utilizatorii
	fgets(buf, 31, f);
	sscanf_s(buf, "%d", &nr_pagini);
	fgets(buf, 31, f);
	sscanf_s(buf, "%d", &nr_utiliz);
	pagini_per_utiliz = (int **)malloc(nr_utiliz * sizeof(int *));
	for (int i = 0; i < nr_utiliz; i++) {
		pagini_per_utiliz[i] = (int *)malloc(nr_pagini * sizeof(int *));
	}
	int i = 0;
	int j = 0;
	while (fgets(buf, 31, f)) {
		int x;
		if (sscanf_s(buf, "%d", &x) == EOF) {
			continue;
		}
		pagini_per_utiliz[i][j] = x;
		j++;
		if (j >= nr_pagini) {
			j = 0;
			i++;
		}
		if (i >= nr_utiliz) {
			break;
		}
	}
	fclose(f);
}

void matr_afis()
{
	if (nr_utiliz == 0) {
		return;
	}
	printf("Pagini deschise per utilizator\n");
	for (int i = 0; i < nr_utiliz; i++) {
		for (int j = 0; j < nr_pagini; j++) {
			printf("%3d ", pagini_per_utiliz[i][j]);
		}
		printf("\n");
	}
	printf("\n");
}


///////////////////////////////////////////////////////////
// arbore binar sortat - nr. accesari aplicatie
///////////////////////////////////////////////////////////

struct accesari_app {
	int index_utiliz;
	int nr_acces;
};

struct arb_nod {
	struct accesari_app acc;
	struct arb_nod *st;
	struct arb_nod *dr;
};

struct arb_nod *rad = NULL;

struct arb_nod *arb_creaza_nod(struct accesari_app acc, struct arb_nod *st, struct arb_nod *dr)
{
	struct arb_nod *an = (struct arb_nod *)malloc(sizeof(struct arb_nod));
	an->acc = acc;
	an->st = st;
	an->dr = dr;
	return an;
}

struct arb_nod *arb_inser(struct arb_nod *rad, struct accesari_app acc)
{
	if (rad == NULL) {
		rad = arb_creaza_nod(acc, NULL, NULL);
		return rad;
	}

	if (acc.nr_acces < rad->acc.nr_acces) {
		rad->st = arb_inser(rad->st, acc);
		return rad;
	}

	rad->dr = arb_inser(rad->dr, acc);
	return rad;
}

void arb_scd(struct arb_nod *rad)
{
	if (rad == NULL) {
		return;
	}
	if (rad->st != NULL) {
		arb_scd(rad->st);
	}
	printf("utiliz %d: %d accesari\n", rad->acc.index_utiliz, rad->acc.nr_acces);
	if (rad->dr != NULL) {
		arb_scd(rad->dr);
	}
}

void arb_afis() 
{
	printf("Utilizatorii sortati in ordinea nr. de accesari aplicatie\n");
	arb_scd(rad);
	printf("\n");
}

void arb_citire(char *fname)
{
	FILE *f = NULL;
	if (0 != fopen_s(&f, fname, "rt")) {
		printf("Nu pot deschide fisierul %s\n", fname);
		return;
	}
	char buf[16];
	struct accesari_app acc;
	// fisierul are formatul: fiecare linie - de cate ori a accesat un utilizator applicatia
	while (fgets(buf, 31, f)) {
		sscanf_s(buf, "%d %d", &(acc.index_utiliz), &(acc.nr_acces));
		rad = arb_inser(rad, acc);
	}
}

int main()
{
	lista_citire("C:\\trafic.txt");
	lista_afis();

	matr_citire("C:\\pagini.txt");
	matr_afis();

	arb_citire("C:\\accesari.txt");
	arb_afis();
}
