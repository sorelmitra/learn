CXX = clang++
CXXFLAGS = -std=c++17 -Wall -Wextra
LDFLAGS = -framework ApplicationServices -framework AppKit

TARGET = keyremap

all: $(TARGET)

$(TARGET): main.mm
	$(CXX) $(CXXFLAGS) -o $(TARGET) main.mm $(LDFLAGS)

clean:
	rm -f $(TARGET)
