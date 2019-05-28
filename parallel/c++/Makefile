VPATH=.:./lib # for building in the current dir
# VPATH=..:../lib # for building in a 'build' sub-directory

RM=rm -vf

CXX=g++
CXXFLAGS=-std=c++0x -W -Wall -Wextra -pedantic -g

O_COMMON=Thread.o 

COMM_THREAD=comm_thread
O_COMM_THREAD=CommunicatingThread.o $(O_COMMON)

DINING_PHIL=dining_phil
O_DINING_PHIL=DiningPhilosopher.o $(O_COMMON)

MERGE_SORT=merge_sort
O_MERGE_SORT=MergeSort.o $(O_COMMON)

all: $(COMM_THREAD) $(DINING_PHIL) $(MERGE_SORT)

$(COMM_THREAD): $(O_COMM_THREAD) -lpthread
	$(CXX) $(CXXFLAGS) $^ -o $@

clean_$(COMM_THREAD):
	$(RM) $(COMM_THREAD) $(O_COMM_THREAD)

$(DINING_PHIL): $(O_DINING_PHIL) -lpthread
	$(CXX) $(CXXFLAGS) $^ -o $@

clean_$(DINING_PHIL):
	$(RM) $(DINING_PHIL) $(O_DINING_PHIL)

$(MERGE_SORT): $(O_MERGE_SORT) -lpthread
	$(CXX) $(CXXFLAGS) $^ -o $@

clean_$(MERGE_SORT):
	$(RM) $(MERGE_SORT) $(O_MERGE_SORT)

clean: clean_$(COMM_THREAD) clean_$(DINING_PHIL) clean_$(MERGE_SORT)
