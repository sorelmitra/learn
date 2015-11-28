// GreenDrive.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "GreenDrive.h"

#include <iostream>
#include <fstream>
#include <cctype>
#include <string>

using namespace std;


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

	enum ParseStates {WAIT, BEGIN_AUTO, IN_AUTO};
	ParseStates state = WAIT;
	string line;
	while (getline(ifs, line)) {

	}
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

ostream& operator<<(ostream& os, const Car &car)
{
	os << endl;
	os << "Nume autovehicul:    " << car.getName().c_str() << endl;

	os << "Motor:               ";
	switch (car.getEngineType()) {
	case ENGINE_TYPE_GASOLINE: os << "Benzina";
	case ENGINE_TYPE_DIESEL: os << "Diesel";
	case ENGINE_TYPE_HYBRID: os << "Hibrid";
	case ENGINE_TYPE_ELECTRICAL: os << "Electric";
	default: cout << "Necunoscut!";
	}
	os << endl;

	os << "Viteza maxima:       " << car.getMaxSpeed() << endl;
	os << "Cilindree:           " << car.getDisplacement() << endl;
	os << "Consum mediu urban:  " << car.getUrbanAverageConsumption() << endl;
	os << "Viteza medie urbana: " << car.getUrbanAverageSpeed() << endl;
	os << "Consum mediu:        " << car.getAverageConsumption() << endl;
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
	char c;
	while (true) {
		prepareMenu("Meniu Principal");
		cout << "A - Selectie autovehicul" << endl;
		cout << "I - Iesire" << endl;
		showMenuInput();
		cin >> c;
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
		cout << "Nu avem autovehicule in aplicatie." << endl;
	}
}

void Menu::showMenuInput()
{
	cout << "Alegeti optiunea > ";	
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
		showMenuInput();
		
		char c;
		cin >> c;
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
	char c;
	while (true) {
		prepareMenu(string("Autovehicul ") + cars[carIndex].getName());
		cout << "A - Afisare date autovehicul" << endl;
		cout << "T - Afisare trasee autovehicul" << endl;
		cout << "I - Iesire" << endl;
		showMenuInput();
		cin >> c;
		switch (tolower(c)) {
		case 'a': cout << cars[carIndex]; break;
		case 't': carRoutes(carIndex); break;
		case 'i': return;
		}
	}
}

void Menu::carRoutes(size_t carIndex)
{
	cout << endl << "NOT IMPLEMENTED!" << endl;
}


///// Main function /////

int main()
{
	Menu m;
	return m.run();
}
