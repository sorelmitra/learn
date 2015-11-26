#pragma once

class MenuItem {
public:
	MenuItem(char accessKey, char *title) {
		this->accessKey = accessKey;
		this->title = title;
	}

private:
	char accessKey;
	char *title;
};