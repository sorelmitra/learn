# Target executables
EXE_AV_CLIENT ?= av-client
EXE_AV_SERVER ?= av-server

# Build and Source directories
BUILD_DIR ?= ./build
SRC_DIRS ?= ./src

# Common sources
SRCS_COMMON := $(shell find $(SRC_DIRS)/common -name *.cpp -or -name *.c -or -name *.s -or -name *.proto)

# Sources per target EXE_AV_CLIENT
SRCS_AV_CLIENT := $(SRCS_COMMON) $(shell find $(SRC_DIRS)/av-client -name *.cpp -or -name *.c -or -name *.s)
OBJS_AV_CLIENT := $(SRCS_AV_CLIENT:%=$(BUILD_DIR)/%.o)
DEPS_AV_CLIENT := $(OBJS_AV_CLIENT:.o=.d)

# Sources per target EXE_AV_SERVER
SRCS_AV_SERVER := $(SRCS_COMMON) $(shell find $(SRC_DIRS)/av-server -name *.cpp -or -name *.c -or -name *.s)
OBJS_AV_SERVER := $(SRCS_AV_SERVER:%=$(BUILD_DIR)/%.o)
DEPS_AV_SERVER := $(OBJS_AV_SERVER:.o=.d)

# Google Protocol Buffers compiler
GPBC := /usr/local/bin/protoc
GPBC_FLAGS :=

INC_DIRS := $(shell find $(SRC_DIRS) -type d | grep -v git)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

CPPFLAGS ?= $(INC_FLAGS) -MMD -MP
CXXFLAGS = -g -std=c++11 -Wall -pedantic
LDFLAGS += -lprotobuf -lboost_filesystem -lboost_system

all: $(BUILD_DIR)/$(EXE_AV_CLIENT) $(BUILD_DIR)/$(EXE_AV_SERVER)

$(BUILD_DIR)/$(EXE_AV_CLIENT): $(OBJS_AV_CLIENT)
	$(CXX) $(OBJS_AV_CLIENT) -o $@ $(LDFLAGS)

$(BUILD_DIR)/$(EXE_AV_SERVER): $(OBJS_AV_SERVER)
	$(CXX) $(OBJS_AV_SERVER) -o $@ $(LDFLAGS)

# assembly
$(BUILD_DIR)/%.s.o: %.s
	$(MKDIR_P) $(dir $@)
	$(AS) $(ASFLAGS) -c $< -o $@

# c source
$(BUILD_DIR)/%.c.o: %.c
	$(MKDIR_P) $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

# c++ source
$(BUILD_DIR)/%.cpp.o: %.cpp
	$(MKDIR_P) $(dir $@)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

# Google Protocol Buffers source
$(BUILD_DIR)/%.proto.o: %.proto
	$(MKDIR_P) $(dir $@)
	$(GPBC) $(GPBC_FLAGS) -I=$(dir $<) --cpp_out=$(dir $@) $?
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(BUILD_DIR)/$*.pb.cc -o $@

.PHONY: clean

clean:
	$(RM) -r $(BUILD_DIR)

-include $(DEPS_AV_CLIENT)

MKDIR_P ?= mkdir -p
