#pragma once

#include <iostream>

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

	friend std::ostream& operator<<(std::ostream& os, const Car &car);

	void setName(char *name);
	char *getName() const;
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

private:
	char *name;
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
	void prepareMenu(char *title);
	void showMenuInput();
	void selectCar();
	void carOptions(size_t carIndex);
	void carRoutes(size_t carIndex);
private:
	size_t carsCount;
	Car *cars;
};
