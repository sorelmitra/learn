// GreenDrive.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "GreenDrive.h"

#include <iostream>
#include <fstream>
#include <cctype>
#include <string>
#include <iomanip>

using namespace std;


///// Parser /////

bool Parser::isEmpty(const string & line)
{
	return (string::npos == line.find_first_not_of(" \t\n"));
}

bool Parser::parseProperty(const string & line, string & prop, string & value)
{
	size_t colonPos = line.find_first_of(":");
	if (string::npos == colonPos) {
		return false;
	}

	prop = trim(line.substr(0, colonPos));
	value = trim(line.substr(colonPos + 1));
	return true;
}

std::string Parser::trimLeft(std::string s)
{
	size_t pos = s.find_first_not_of(" \t\n");
	if (string::npos == pos) {
		return std::string();
	}

	return s.substr(pos);
}

std::string Parser::trimRight(std::string s)
{
	size_t pos = s.find_last_not_of(" \t\n");
	if (string::npos == pos) {
		return std::string();
	}

	return s.substr(0, pos + 1);
}

std::string Parser::trim(std::string s)
{
	return trimRight(trimLeft(s));
}


///// Car /////

Car::Car()
{
	name = "";
	engineType = ENGINE_TYPE_UNDEFINED;
	maxSpeed = 0;
	displacement = 0;
	urbanAverageConsumption = 0;
	urbanAverageSpeed = 0;
	averageConsumption = 0;
	averageSpeed = 0;
}

Car::~Car()
{
}

void Car::parse(std::string fname, Car *& cars, size_t & carsCount)
{
	ifstream ifs(fname);
	if (!ifs.is_open()) {
		throw runtime_error(string("Nu pot citi din fisierul '") + fname + "'");
	}

	int n = countCars(fname);
	cars = new Car[n];

	string line;
	int i = -1;
	while ((i < n) && getline(ifs, line)) {
		string prop, value;
		if (Parser::isEmpty(line)) {
			continue;
		}

		if (!Parser::parseProperty(line, prop, value)) {
			i++;
			cars[i].setName(Parser::trim(line));
			continue;
		}

		if (0 == prop.compare("engine")) {
			cars[i].setEngineType(value);
		}
		else if (0 == prop.compare("max_speed")) {
			cars[i].setMaxSpeed(value);
		}
		else if (0 == prop.compare("engine_cc")) {
			cars[i].setDisplacement(value);
		}
		else if (0 == prop.compare("avg_consumption_urban")) {
			cars[i].setUrbanAverageConsumption(value);
		}
		else if (0 == prop.compare("avg_speed_urban")) {
			cars[i].setUrbanAverageSpeed(value);
		}
		else if (0 == prop.compare("avg_consumption")) {
			cars[i].setAverageConsumption(value);
		}
		else if (0 == prop.compare("avg_speed")) {
			cars[i].setAverageSpeed(value);
		}
		else {
			cout << "EROARE: Proprietate autovehicul necunoscuta <" << prop << ">" << endl;
		}
	}

	carsCount = i + 1;
	cout << "S-au citit " << carsCount << " autovehicule din " << n << endl;
}

void Car::setName(string name)
{
	this->name = name;
}

string Car::getName() const
{
	return name;
}

void Car::setEngineType(CarEngineTypes engineType)
{
	this->engineType = engineType;
}

CarEngineTypes Car::getEngineType() const
{
	return engineType;
}

void Car::setMaxSpeed(unsigned short maxSpeed)
{
	this->maxSpeed = maxSpeed;
}

unsigned short Car::getMaxSpeed() const
{
	return maxSpeed;
}

void Car::setDisplacement(unsigned short displacement)
{
	this->displacement = displacement;
}

unsigned short Car::getDisplacement() const
{
	return displacement;
}

void Car::setUrbanAverageConsumption(unsigned char urbanAverageConsumption)
{
	this->urbanAverageConsumption = urbanAverageConsumption;
}

unsigned char Car::getUrbanAverageConsumption() const
{
	return urbanAverageConsumption;
}

void Car::setUrbanAverageSpeed(unsigned short urbanAverageSpeed)
{
	this->urbanAverageSpeed = urbanAverageSpeed;
}

unsigned short Car::getUrbanAverageSpeed() const
{
	return urbanAverageSpeed;
}

void Car::setAverageConsumption(unsigned char averageConsumption)
{
	this->averageConsumption = averageConsumption;
}

unsigned char Car::getAverageConsumption() const
{
	return averageConsumption;
}

void Car::setAverageSpeed(unsigned short averageSpeed)
{
	this->averageSpeed = averageSpeed;
}

unsigned short Car::getAverageSpeed() const
{
	return averageSpeed;
}

// "Setters" that parse data

void Car::setEngineType(std::string str)
{
	if (0 == str.compare("gasoline")) {
		this->engineType = ENGINE_TYPE_GASOLINE;
	} else if (0 == str.compare("diesel")) {
		this->engineType = ENGINE_TYPE_DIESEL;
	} else if (0 == str.compare("hybrid")) {
		this->engineType = ENGINE_TYPE_HYBRID;
	} else if (0 == str.compare("electrical")) {
		this->engineType = ENGINE_TYPE_ELECTRICAL;
	} else {
		this->engineType = ENGINE_TYPE_UNDEFINED;
		cout << "EROARE: Tip motor necunoscut <" << str << ">" << endl;
	}
}

void Car::setMaxSpeed(std::string str)
{
	this->maxSpeed = std::stoi(str);
}

void Car::setDisplacement(std::string str)
{
	this->displacement = std::stoi(str);
}

void Car::setUrbanAverageConsumption(std::string str)
{
	this->urbanAverageConsumption = std::stoi(str);
}

void Car::setUrbanAverageSpeed(std::string str)
{
	this->urbanAverageSpeed = std::stoi(str);
}

void Car::setAverageConsumption(std::string str)
{
	this->averageConsumption = std::stoi(str);
}

void Car::setAverageSpeed(std::string str)
{
	this->averageSpeed = std::stoi(str);
}

int Car::countCars(std::string fname)
{
	ifstream ifs(fname);
	if (!ifs.is_open()) {
		throw runtime_error(string("Nu pot citi din fisierul '") + fname + "'");
	}

	int n = 0;
	string line;
	while (getline(ifs, line)) {
		string prop, value;
		if (Parser::isEmpty(line)) {
			continue;
		}
		// Any line that is not a property and is not empty is a car name, thus increase cars count
		if (!Parser::parseProperty(line, prop, value)) {
			n++;
		}
	}

	return n;
}

ostream& operator<<(ostream& os, const Car &car)
{
	os << endl;
	os << "Nume autovehicul:    " << car.getName().c_str() << endl;

	os << "Motor:               ";
	switch (car.getEngineType()) {
	case ENGINE_TYPE_GASOLINE: os << "Benzina"; break;
	case ENGINE_TYPE_DIESEL: os << "Diesel"; break;
	case ENGINE_TYPE_HYBRID: os << "Hibrid"; break;
	case ENGINE_TYPE_ELECTRICAL: os << "Electric"; break;
	default: cout << "Necunoscut!"; break;
	}
	os << endl;

	os << "Viteza maxima:       " << car.getMaxSpeed() << endl;
	os << "Cilindree:           " << car.getDisplacement() << endl;
	os << "Consum mediu urban:  " << (int)car.getUrbanAverageConsumption() << endl;
	os << "Viteza medie urbana: " << car.getUrbanAverageSpeed() << endl;
	os << "Consum mediu:        " << (int)car.getAverageConsumption() << endl;
	os << "Viteza medie:        " << car.getAverageSpeed() << endl;
	os << endl;

	return os;
}


///// Main Menu /////

Menu::Menu()
{
	cars = 0;
	carsCount = 0;
}

int Menu::run()
{
	readRequiredData();
	while (true) {
		prepareMenu("Meniu Principal");
		cout << "A - Selectie autovehicul" << endl;
		cout << "I - Iesire" << endl;
		char c = readUserInput();
		switch(tolower(c)) {
		case 'a': selectCar(); break;
		case 'i': return 0;
		}
	}
}

void Menu::readRequiredData()
{
	try {
		Car::parse("auto.txt", cars, carsCount);
	}
	catch (runtime_error e) {
		cout << e.what() << endl;
	}
}

void Menu::showMenuInput()
{
	cout << "Alegeti optiunea > ";	
}

char Menu::readUserInput()
{
	showMenuInput();
	char c;
	cin >> c;
	cout << endl << "==========================================" << endl << endl;
	return c;
}

void Menu::prepareMenu(string title)
{
	cout << endl;
	cout << "===== " << title.c_str() << " =====" << endl;
	cout << endl;
}

void Menu::selectCar()
{
	while (true) {
		prepareMenu("Selectie Autovehicule");
		char opt = '0';
		size_t i;
		for (i = 0; i < carsCount; i++) {
			cout << opt++ << " - " << cars[i].getName().c_str() << endl;
		}
		cout << "I - Iesire" << endl;
		char c = readUserInput();

		if (c == 'i') return;
		opt = '0';
		for (i = 0; i < carsCount; i++) {
			if (c == opt++) break;
		}
		
		if (i >= carsCount) {
			cout << "Va rugam selectati o optiune valida sau I pentru iesire" << endl;
			continue;
		}
		
		carOptions(i);
	}
}

void Menu::carOptions(size_t carIndex)
{
	while (true) {
		prepareMenu(string("Autovehicul ") + cars[carIndex].getName());
		cout << "A - Afisare date autovehicul" << endl;
		cout << "T - Afisare trasee autovehicul" << endl;
		cout << "I - Iesire" << endl;
		char c = readUserInput();
		switch (tolower(c)) {
		case 'a': cout << cars[carIndex]; break;
		case 't': carRoutes(carIndex); break;
		case 'i': return;
		}
	}
}

void Menu::carRoutes(size_t carIndex)
{
	cout << endl << "In dezvoltare ... " << endl << endl;
}


///// Main function /////

int main()
{
	Menu m;
	return m.run();
}
