// GreenDrive.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "GreenDrive.h"

#include <iostream>
#include <cctype>

///// Main Menu /////

int Menu::run()
{
	char c;
	while (true) {
		show();
		cin >> c;
		switch(tolower(c)) {
		case 'a': selectCar(); break;
		case 'i': return 0;
		}
	}
}

void Menu::showInput()
{
	cout << "Alegeti optiunea > ";	
}

void Menu::prepareShow()
{
	cout << endl;
}

void Menu::show()
{
	prepareShow();
	cout << "A - Selectie autovehicul" << endl;
	cout << "I - Iesire" << endl;
	showInput();
}

void Menu::selectCar()
{
	while (true) {
		prepareShow();
		char opt = '0';
		for (size_t i = 0; i < carsCount; i++) {
			cout << opt++ << " - " cars[i].name << endl;
		}
		cout << "I - Iesire" << endl;
		showInput();
		
		char c;
		cin >> c;
		if (c == 'i') return;
		opt = '0';
		for (size_t i = 0; i < carsCount; i++) {
			if (c == opt++) break;
		}
		
		if (i >= carsCount) {
			cout << "Va rugam selectati o optiune valida sau I pentru iesire" << endl;
			continue;
		}
		
		showCarOptions();
	}
}


///// Main function /////

int main()
{
	Menu m;
	return m.run();
}

