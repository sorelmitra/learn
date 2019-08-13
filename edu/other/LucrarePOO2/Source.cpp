#include <string.h>
#include <iostream>
#include <ostream>
#include <cctype>

// Din cauza unui warning de Visual Studio 2015
#define strcpy(d,s) (memcpy(d, s, strlen(s)))
//#define strcpy(d,s) (strcpy_s(d, strlen(s), s))
#define strncpy(d,s,n) (strncpy_s(d, n, s, n))

using namespace std;

class SalaSeminar {
public:
	SalaSeminar();
	SalaSeminar(const char *cod, unsigned short locuri, bool areProiector);
	SalaSeminar(const SalaSeminar &other);
	~SalaSeminar();

	void setCod(const char *cod);
	char *getCod() const; // const, adica metoda nu modifica obiectul

	bool operator>(const SalaSeminar &other);
	SalaSeminar &operator=(const SalaSeminar &other);
	SalaSeminar &operator!();

	friend istream &operator>>(istream &is, SalaSeminar &ss);
	friend ostream &operator<<(ostream &is, const SalaSeminar &ss);

	friend unsigned short operator+(unsigned short nr, const SalaSeminar &ss);

private:
	char *cod_clasa;
	unsigned short nr_locuri;
	bool are_proiector;
};

SalaSeminar::SalaSeminar(): nr_locuri(0), are_proiector(false), cod_clasa(0)
{
}

SalaSeminar::SalaSeminar(const char * cod, unsigned short locuri, bool areProiector): nr_locuri(locuri), are_proiector(areProiector)
{
	setCod(cod);
}

SalaSeminar::SalaSeminar(const SalaSeminar & other)
{
	cod_clasa = new char[strlen(other.cod_clasa) + 1];
	strcpy(cod_clasa, other.cod_clasa);

	nr_locuri = other.nr_locuri;
	are_proiector = other.are_proiector;
}

SalaSeminar::~SalaSeminar()
{
	if (cod_clasa != 0) {
		delete[] cod_clasa;
		cod_clasa = 0;
	}
}

void SalaSeminar::setCod(const char * cod)
{
	if (cod_clasa != 0) {
		delete[] cod_clasa;
	}

	size_t n = strlen(cod);
	cod_clasa = new char[n + 1];
	strcpy(cod_clasa, cod);
	cod_clasa[n] = 0;
}

char * SalaSeminar::getCod() const
{
	return cod_clasa;
}

bool SalaSeminar::operator>(const SalaSeminar & other)
{
	return nr_locuri > other.nr_locuri;
}

SalaSeminar & SalaSeminar::operator=(const SalaSeminar & other)
{
	// Previne copiere in sine insusi
	if (this == &other) {
		return *this;
	}

	setCod(other.getCod());
	nr_locuri = other.nr_locuri;
	are_proiector = other.are_proiector;

	return *this;
}

SalaSeminar & SalaSeminar::operator!()
{
	are_proiector = !are_proiector;

	return *this;
}

istream &operator>>(istream &is, SalaSeminar &ss)
{
	ss.cod_clasa = new char[100];
	is >> ss.cod_clasa;

	is >> ss.nr_locuri;

	char c;
	is >> c;
	ss.are_proiector = (tolower(c) == 'y');

	return is;
}

ostream &operator<<(ostream &os, const SalaSeminar &ss)
{
	os << "Sala " << ss.getCod() << ": " << ss.nr_locuri << " locuri, " << (ss.are_proiector ? "": "nu") << " are proiector" << endl;

	return os;
}

unsigned short operator+(unsigned short nr, const SalaSeminar &ss)
{
	return nr + ss.nr_locuri;
}

int main() {
	SalaSeminar ss1("Amfiteatru", 50, false);
	SalaSeminar ss2;
	
	cout << "Introduceti sala 2 (nume, locuri, are proiector Y/N): " << endl;
	cin >> ss2;

	cout << "Salile sunt: " << endl;
	cout << ss1 << endl << ss2 << endl;
	
	SalaSeminar ss3;
	ss3 = ss2;
	ss3.setCod((string("Clona ") + ss2.getCod()).c_str()); // Creaza un std::string "Clona ", il concateneaza cu numele lui ss2 si obtine un char * din el
	ss3 = !ss3; cout << ss3 << endl;
	int nr_locuri = 10 + ss3; cout << "Nr locuri calculate: " << nr_locuri << endl;

	cout << "Sala " << ss2.getCod() << " este " << (ss2 > ss1 ? "mai mare" : "mai mica sau egala") << " decat sala " << ss1.getCod() << endl;

	char c;
	cout << endl << "Tastati 0 ENTER pentru a iesi > "; cin >> c;

	return 0;
}
