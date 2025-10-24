APP_NAME := KeyRemap
BUNDLE_ID := com.example.KeyRemap
APP_VERSION := 1.0

CXX := clang++
CXXFLAGS := -std=c++17 -Wall -Wextra
LDFLAGS := -framework ApplicationServices -framework AppKit

APP_DIR := $(APP_NAME).app
CONTENTS := $(APP_DIR)/Contents
MACOS := $(CONTENTS)/MacOS
RES := $(CONTENTS)/Resources
PLIST := $(CONTENTS)/Info.plist
ICON := $(RES)/AppIcon.icns
BIN := $(MACOS)/$(APP_NAME)

all: $(APP_DIR)

$(APP_DIR): $(BIN) $(PLIST) $(ICON)
	@echo "Built $(APP_DIR)"

$(BIN): main.mm | $(MACOS)
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDFLAGS)
	chmod +x $@

$(PLIST): | $(CONTENTS)
	@echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" > $@
	@echo "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">" >> $@
	@echo "<plist version=\"1.0\">" >> $@
	@echo "<dict>" >> $@
	@echo "\t<key>CFBundleName</key>\n\t<string>$(APP_NAME)</string>" >> $@
	@echo "\t<key>CFBundleDisplayName</key>\n\t<string>$(APP_NAME)</string>" >> $@
	@echo "\t<key>CFBundleIdentifier</key>\n\t<string>$(BUNDLE_ID)</string>" >> $@
	@echo "\t<key>CFBundleVersion</key>\n\t<string>$(APP_VERSION)</string>" >> $@
	@echo "\t<key>CFBundleShortVersionString</key>\n\t<string>$(APP_VERSION)</string>" >> $@
	@echo "\t<key>CFBundlePackageType</key>\n\t<string>APPL</string>" >> $@
	@echo "\t<key>CFBundleExecutable</key>\n\t<string>$(APP_NAME)</string>" >> $@
	@echo "\t<key>LSMinimumSystemVersion</key>\n\t<string>10.13</string>" >> $@
	@echo "\t<key>LSUIElement</key>\n\t<true/>" >> $@
	@echo "\t<key>NSHighResolutionCapable</key>\n\t<true/>" >> $@
	@echo "\t<key>CFBundleIconFile</key>\n\t<string>AppIcon</string>" >> $@
	@echo "</dict>\n</plist>" >> $@

$(ICON): | $(RES)
	# Minimal placeholder icon (valid but generic ICNS header)
	echo -n 'icns' > $@
	echo -n '\x00\x00\x00\x0c' >> $@
	echo -n 'fkmk' >> $@
	echo -n '\x00\x00\x00\x00' >> $@

$(MACOS) $(RES) $(CONTENTS):
	mkdir -p $@

clean:
	rm -rf $(APP_DIR)

.PHONY: all clean
