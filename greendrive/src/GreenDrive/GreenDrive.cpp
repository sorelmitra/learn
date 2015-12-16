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


///// CarPosition /////

CarPosition::CarPosition()
	: carName(""), milliseconds(0), latitude(0), longitude(0), speed(0)
{
}

CarPosition::~CarPosition()
{
}

std::string CarPosition::getCarName() const
{
	return carName;
}

void CarPosition::setCarName(std::string carName)
{
	this->carName = carName;
}

long long CarPosition::getMilliseconds() const
{
	return milliseconds;
}

void CarPosition::setMilliseconds(long long milliseconds)
{
	this->milliseconds = milliseconds;
}

double CarPosition::getLatitude() const
{
	return latitude;
}

void CarPosition::setLatitude(double latitude)
{
	this->latitude = latitude;
}

double CarPosition::getLongitude() const
{
	return longitude;
}

void CarPosition::setLongitude(double longitude)
{
	this->longitude = longitude;
}

unsigned short CarPosition::getSpeed() const
{
	return speed;
}

void CarPosition::setSpeed(unsigned short speed)
{
	this->speed = speed;
}

ostream &operator<<(ostream &os, const CarPosition carPos)
{
	os << "Autovehicul:   " << carPos.getCarName() << endl;


	os << "Timp (ms):     " << carPos.getMilliseconds() << endl;

	os << "Latitudine:    " << setprecision(7) << carPos.getLatitude() << endl;
	os << "Longitudine:   " << setprecision(7) << carPos.getLongitude() << endl;
	os << "Viteza:        " << carPos.getSpeed() << endl;

	return os;
}


///// CarRouteSegment /////

CarRouteSegment::CarRouteSegment()
{
}

CarRouteSegment::~CarRouteSegment()
{
}

CarPosition CarRouteSegment::getStart()
{
	return start;
}

void CarRouteSegment::setStart(CarPosition pos)
{
	start = pos;
}

CarPosition CarRouteSegment::getEnd()
{
	return end;
}

void CarRouteSegment::setEnd(CarPosition pos)
{
	end = pos;
}

std::ostream &operator<<(std::ostream &os, CarRouteSegment &segment)
{
	os << "Viteza " << segment.start.getSpeed() << " km/h";
	os << "; distanta (" << setprecision(7) << segment.end.getLatitude() - segment.start.getLatitude() << ", " << segment.end.getLongitude() - segment.start.getLongitude() << ") grade";
	os << "; durata " << (segment.end.getMilliseconds() - segment.start.getMilliseconds()) / 1000 << " sec";
	os << endl;
	return os;
}


///// CarRoute::iterator /////

CarRoute::iterator::iterator(CarRoute &route, std::list<CarPosition>::iterator posIt)
	: posIt(posIt), route(route)
{
	if (posIt == route.positions.end()) {
		return;
	}

	(*this)++;
}

CarRoute::iterator &CarRoute::iterator::operator++(int)
{
	segment.setStart(*posIt);
	segment.setEnd(segment.getStart());
	for (; posIt != route.positions.end(); posIt++) {
		CarPosition pos = *posIt;
		if (pos.getSpeed() != segment.getStart().getSpeed()) {
			break;
		}
		segment.setEnd(pos);
	}
	return *this;
}

bool CarRoute::iterator::operator!=(iterator other)
{
	return posIt != other.posIt;
}

CarRouteSegment CarRoute::iterator::operator*()
{
	return segment;
}


///// CarRoute /////

CarRoute::CarRoute()
{
	positions = list<CarPosition>();
}

CarRoute::~CarRoute()
{
}

void CarRoute::parse(std::string fname, Car * cars, size_t & carsCount)
{
	ifstream ifs(fname);
	if (!ifs.is_open()) {
		throw runtime_error(string("Nu pot citi din fisierul '") + fname + "'");
	}

	cout << "Citim datele despre trasee ";

	enum CAR_POS_STATE {CPS_NAME, CPS_TIME, CPS_LAT, CPS_LONG, CPS_SPEED };
	string line;
	CarPosition carPos;
	CAR_POS_STATE state = CPS_NAME;
	int n = 0;
	while (getline(ifs, line)) {
		switch (state)
		{
		case CPS_NAME:
			carPos.setCarName(Parser::trim(line));
			state = CPS_TIME;
			continue;
		case CPS_TIME:
			carPos.setMilliseconds(stoll(line));
			state = CPS_LAT;
			continue;
		case CPS_LAT:
			carPos.setLatitude(stod(line));
			state = CPS_LONG;
			continue;
		case CPS_LONG:
			carPos.setLongitude(stod(line));
			state = CPS_SPEED;
			continue;
		case CPS_SPEED:
			carPos.setSpeed(stoi(line));
			state = CPS_NAME;
			break;
		default:
			cout << "EROARE: Stare necunoscuta " << state << endl;
			continue;
		}

		Car *car = getCarByName(carPos.getCarName(), cars, carsCount);
		if (car == 0) {
			cout << "EROARE: Am pozitia unui autovehicul pe care nu-l am in lista: <" << carPos.getCarName() << ">" << endl;
			continue;
		}
		car->getRoute().positions.push_back(carPos);
		n++;

		if (n % 10000 == 0) {
			cout << ".";
		}
	}

	cout << endl;
	cout << "Am citit in total " << n << " pozitii pentru toate autovehiculele." << endl;
}

std::list<CarPosition> &CarRoute::getPositions()
{
	return positions;
}

CarRoute::iterator CarRoute::constantSpeedSegmentsBegin()
{
	return CarRoute::iterator(*this, positions.begin());
}

CarRoute::iterator CarRoute::constantSpeedSegmentsEnd()
{
	return CarRoute::iterator(*this, positions.end());
}

Car * CarRoute::getCarByName(std::string name, Car * cars, size_t & carsCount)
{
	for (size_t i = 0; i < carsCount; i++) {
		if (0 == name.compare(cars[i].getName())) {
			return cars + i;
		}
	}

	return 0;
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

CarRoute &Car::getRoute()
{
	return route;
}

void Car::setRoute(CarRoute route)
{
	this->route = route;
}

// Parsing "setters"

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
		CarRoute::parse("telematics.txt", cars, carsCount);
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
	cout << endl << "Afisam traseul pe segmente cu viteza constanta " << endl << endl;

	CarRoute &route = cars[carIndex].getRoute();
	int n = 0;
	for (CarRoute::iterator it = route.constantSpeedSegmentsBegin(); it != route.constantSpeedSegmentsEnd(); it++) {
		CarRouteSegment segment = *it;
		cout << "Segment " << ++n << ": " << segment << endl;
	}

	cout << endl << "Mai e de IMPLEMENTAT!" << endl;
}


///// Main function /////

int main()
{
	Menu m;
	return m.run();
}
