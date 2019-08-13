#include <string.h>
#include <iostream>
#include <ostream>

using namespace std;

#define strcpy(d,s) (strcpy_s(d, strlen(s), s))

class Jurnal {
public:

	// Nota 5

	Jurnal(); // constructor explicit fara parametri
	Jurnal(const int _ISSN, char *denumire); // constructor cu parametri

	char *getDenumire() {
		return denumire;
	}

	const int getISSN() {
		return ISSN;
	}

	// Nota 6

	~Jurnal(); // destructor
	Jurnal(const Jurnal &j); // constructor de copiere
	Jurnal &operator=(const Jurnal &j); // operator =

	// Nota 7

	int TotalArticole();
	friend ostream &operator<<(ostream &os, const Jurnal &j);

	// Nota 8

	float operator+=(float f);

	// Nota 9

	int operator()(); // operator cast
	friend istream &operator>>(istream &is, Jurnal &j);

	// Nota 10

	int operator+=(int n);

private:

	// Nota 3

	char *denumire;
	int nr_numere;
	int *articole_pe_numar;
	const int ISSN = 0;
	float pret;
};

// constructor fara parametri
Jurnal::Jurnal(): ISSN(0) { // const int poate fi asignat doar asa
	denumire = 0;
	nr_numere = 0;
	articole_pe_numar = 0;
	pret = 0;
}

// constructor cu parametri
Jurnal::Jurnal(const int _ISSN, char *_denumire): ISSN(_ISSN) {
	denumire = new char[strlen(_denumire) + 1]; // +1 pentru 0 de la final
	strcpy(denumire, _denumire);
}

// destructor
Jurnal::~Jurnal()
{
	if (denumire) {
		delete[] denumire;
		denumire = 0;
	}

	if (articole_pe_numar) {
		delete[] articole_pe_numar;
		articole_pe_numar = 0;
	}
}

// constructor de copiere
Jurnal::Jurnal(const Jurnal &j): ISSN(j.ISSN)
{
	denumire = new char[strlen(j.denumire) + 1]; // +1 pentru 0 de la final
	strcpy(denumire, j.denumire);

	nr_numere = j.nr_numere;

	if (nr_numere > 0) {
		articole_pe_numar = new int[nr_numere];
		for (int i = 0; i < nr_numere; i++) {
			articole_pe_numar[i] = j.articole_pe_numar[i];
		}
	}

	pret = j.pret;
}

Jurnal & Jurnal::operator=(const Jurnal & j)
{
	denumire = new char[strlen(j.denumire) + 1]; // +1 pentru 0 de la final
	strcpy(denumire, j.denumire);

	nr_numere = j.nr_numere;

	if (nr_numere > 0) {
		articole_pe_numar = new int[nr_numere];
		for (int i = 0; i < nr_numere; i++) {
			articole_pe_numar[i] = j.articole_pe_numar[i];
		}
	}

	// ISSN = j.ISSN; // aici e o problema, ISSN e const si nu poate fi modificat nici macar in operator=. Problema e gresita (poate intentionat?)

	pret = j.pret;

	return *this;
}

int Jurnal::TotalArticole()
{
	int tot = 0;
	for (int i = 0; i < nr_numere; i++) {
		tot += articole_pe_numar[i];
	}

	return tot;
}

float Jurnal::operator+=(float f)
{
	pret += f;
	return pret;
}

int Jurnal::operator()()
{
	return TotalArticole();
}

int Jurnal::operator+=(int n)
{
	int *a = new int[nr_numere + 1];
	for (int i = 0; i < nr_numere; i++) {
		a[i] = articole_pe_numar[i];
	}
	if (articole_pe_numar) {
		delete[] articole_pe_numar;
	}
	articole_pe_numar = a;
	articole_pe_numar[nr_numere++] = n;

	return n;
}

ostream &operator<<(ostream &os, const Jurnal &j)
{
	os << "Nume: " << j.denumire << endl;
	os << "Numere: " << j.nr_numere << endl;
	
	os << "Articole per numar: ";
	for (int i = 0; i < j.nr_numere; i++) {
		os << j.articole_pe_numar[i] << " ";
	}
	os << endl;
	
	os << "ISSN: " << j.ISSN << endl;
	os << "Pret: " << j.pret << endl;

	return os;
}

// operatorul citeste de la orice istream; textul problemei e neclar - zice ca vrea de la tastatura; in cazul asta punem si niste cout-uri inainte de fiecare is >>
istream &operator>>(istream &is, Jurnal &j)
{
	j.denumire = new char[100];
	is >> j.denumire;

	is >> j.nr_numere;

	if (j.nr_numere > 0) {
		j.articole_pe_numar = new int[j.nr_numere];
		for (int i = 0; i < j.nr_numere; i++) {
			is >> j.articole_pe_numar[i];
		}
	}

	// is >> j.ISSN; // din cauza const nu merge; text problema gresit
	is >> j.pret;

	return is;
}

int main() {
	int n;
	Jurnal *j;
	cout << "Introduceti nr. de elemente: "; cin >> n;
	j = new Jurnal[n];
	for (int i = 0; i < n; i++) {
		cout << "Introduceti jurnalul " << i << " (nume, numere, articole/numar, pret)" << endl;
		cin >> j[i];
	}

	j[0] += (float)0.5;
	cout << "Am incrementat pretul, jurnalul 0 este acum: " << endl << j[0];
	j[0] += 10;
	cout << "Am adaugat un numar cu 10 articole, jurnalul 0 este acum: " << endl << j[0];

	cout << "Tastati 0 ENTER pentru a iesi >> ";
	cin >> n;

	return 0;
}
