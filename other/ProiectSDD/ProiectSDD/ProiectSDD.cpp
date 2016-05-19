#include "stdafx.h"

#include<stdio.h>
#include<stdlib.h>

struct trafic_date {
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
	}

	struct nod_lista *temp, *right;
	temp = (struct nod_lista *)malloc(sizeof(struct nod_lista));
	temp->trafic = trafic;
	right = (struct nod_lista *)lista_cap;
	while (right->next != NULL)
		right = right->next;
	right->next = temp;
	right = temp;
	right->next = NULL;
}

int lista_sterge(struct trafic_date trafic)
{
	struct nod_lista *temp, *prev;
	temp = lista_cap;
	prev = NULL;
	while (temp != NULL)
	{
		if (temp->trafic.download == trafic.download)
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

void lista_afis(struct nod_lista *r)
{
	r = lista_cap;
	if (r == NULL)
	{
		return;
	}
	while (r != NULL)
	{
		printf("{%d %d}", r->trafic.upload, r->trafic.download);
		r = r->next;
	}
	printf("\n");
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

void lista_citire(char *fname)
{
	FILE *f = NULL;
	if (0 != fopen_s(&f, fname, "rt")) {
		printf("Nu pot deschide fisierul %s", fname);
		return;
	}
	char buf[32];
	struct trafic_date trafic;
	while (fgets(buf, 31, f)) {
		sscanf_s(buf, "%d, %d", &(trafic.upload), &(trafic.download));
		lista_adauga(trafic);
	}
	fclose(f);
}

int main()
{
	lista_citire("C:\\trafic.txt");
	lista_afis(lista_cap);
}
