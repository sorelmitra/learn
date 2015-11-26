#pragma once

#include "MenuItem.h"

class Menu {
public:
	Menu(size_t size);
	~Menu();
	void addItem(char accessKey, char *title);
	int run();

private:
	MenuItem **items;
	size_t size;
	size_t last;
	Menu(Menu &); // Prevent copy construction
	Menu &operator =(Menu &) {} // Prevent assigning
};
