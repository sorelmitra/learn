// GreenDrive.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "Menu.h"

int main()
{
	Menu m(10);
	m.addItem('0', "Exit");
	return m.run();
}

