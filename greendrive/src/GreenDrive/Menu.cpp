#include "stdafx.h"

#include "Menu.h"
#include <stdexcept>

Menu::Menu(size_t size)
{
	items = new MenuItem *[size];
	for (size_t i = 0; i < size; i++) {
		items[i] = 0;
	}
}

Menu::~Menu()
{
	for (size_t i = 0; i < size; i++) {
		if (items[i] == 0) continue;
		delete items[i];
	}
	delete[] items;
}

void Menu::addItem(char accessKey, char *title)
{
	if (last >= size) {
		throw std::runtime_error("Menu full, cannot add more items");
	}
	MenuItem *item = new MenuItem(accessKey, title);
	items[last++] = item;
}

int Menu::run()
{
	throw std::runtime_error("Not implemented");
	return 0;
}

