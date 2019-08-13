#pragma once

#include <iostream>
#include <list>
// #include <WinSock2.h> // for timeval

class Parser
{
public:
	static bool isEmpty(const std::string &line);
	static bool parseProperty(const std::string &line, std::string &prop, std::string &value);
	static std::string trim(std::string s);

private:
	Parser(); // you don't instantiate objects of this type

	static std::string trimLeft(std::string s);
	static std::string trimRight(std::string s);
};

enum CarEngineTypes {
	ENGINE_TYPE_UNDEFINED = 0,

	ENGINE_TYPE_GASOLINE,
	ENGINE_TYPE_DIESEL,
	ENGINE_TYPE_HYBRID,
	ENGINE_TYPE_ELECTRICAL,

	ENGINE_TYPE_MAX
};

class Car {
public:
	Car();
	~Car();

	static void parse(std::string fname, Car *&cars, size_t &carsCount);

	friend std::ostream& operator<<(std::ostream& os, const Car &car);

	void setName(std::string name);
	std::string getName() const;
	void setEngineType(CarEngineTypes engineType);
	CarEngineTypes getEngineType() const;
	void setMaxSpeed(unsigned short maxSpeed);
	unsigned short getMaxSpeed() const;
	void setDisplacement(unsigned short displacement);
	unsigned short getDisplacement() const;
	void setUrbanAverageConsumption(unsigned char urbanAverageConsumption);
	unsigned char getUrbanAverageConsumption() const;
	void setUrbanAverageSpeed(unsigned short urbanAverageSpeed);
	unsigned short getUrbanAverageSpeed() const;
	void setAverageConsumption(unsigned char averageConsumption);
	unsigned char getAverageConsumption() const;
	void setAverageSpeed(unsigned short averageSpeed);
	unsigned short getAverageSpeed() const;

	// Parsing "setters"
	void setEngineType(std::string str);
	void setMaxSpeed(std::string str);
	void setDisplacement(std::string str);
	void setUrbanAverageConsumption(std::string str);
	void setUrbanAverageSpeed(std::string str);
	void setAverageConsumption(std::string str);
	void setAverageSpeed(std::string str);

private:
	static int countCars(std::string fname);

private:
	std::string name;
	CarEngineTypes engineType;
	unsigned short maxSpeed;
	unsigned short displacement;
	unsigned char urbanAverageConsumption;
	unsigned short urbanAverageSpeed;
	unsigned char averageConsumption;
	unsigned short averageSpeed;
};

class Menu {
public:
	Menu();
	int run();

private:
	void readRequiredData();
	void prepareMenu(std::string title);
	void showMenuInput();
	char readUserInput();
	void selectCar();
	void carOptions(size_t carIndex);
	void carRoutes(size_t carIndex);

private:
	size_t carsCount;
	Car *cars;
};
