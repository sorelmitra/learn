#include <pthread.h>
#include <unistd.h>

#include <iostream>
#include <sstream>
#include <memory>

#include "MergeSort.h"

using namespace std;

bool syncThread=true;

pthread_mutex_t mutex=PTHREAD_MUTEX_INITIALIZER;

unsigned int MergeSort::count=0;

std::string MergeSort::toString() {
    std::ostringstream ss;
    ss << "phil " << number;
    return ss.str();
}

void MergeSort::logMsg(std::string s) {
    pthread_mutex_lock(&mutex);
    cerr << toString() << " " << s << endl;
    pthread_mutex_unlock(&mutex);
}

void MergeSort::think() {
    logMsg("thinking");
    usleep(random() % 1000);
}

void MergeSort::eat() {
    stringstream ss;

    logMsg("will eat now");

    needLeftStick=true;
    while (!chops->gain(leftStick)) ;
    needLeftStick=false;
    ss.str(""); ss << "got left stick " << leftStick; logMsg(ss.str());

    needRightStick=true;
    while (!chops->gain(rightStick)) ;
    needRightStick=false;
    ss.str(""); ss << "got right stick " << rightStick; logMsg(ss.str());

    logMsg("eating");
    usleep(random() % 1000);

    chops->release(leftStick);
    ss.str(""); ss << "released left stick " << rightStick; logMsg(ss.str());

    chops->release(rightStick);
    ss.str(""); ss << "released right stick " << rightStick; logMsg(ss.str());
}

void *MergeSort::run() {
    while(1) {
        think();
        eat();
    }
    return NULL;
}

void parse_args(int argc, char **argv) {
    int option=0;
    const char *options="u";
    while ( -1 != (option=getopt(argc, argv, options))) {
        switch(option) {
        case 'u': syncThread=false; break;
        }
    }
}

int main(int argc, char **argv) {
    parse_args(argc, argv);
    const int n=5;
    MergeSort *phils[n];
    Chopsticks chops(n);
    cout << "Creating " << n << " philosophers" << endl;
    for (int i=0; i<n; i++) {
        int l, r;
        if (i==0) {
	   l=i;
	   r=n-1;
        } else {
	   l=i;
	   r=i-1;
        }
        phils[i]=new MergeSort(&chops, l, r, syncThread);
        phils[i]->start();
    }
    for (int i=0; i<n; i++) {
        phils[i]->join();
    }
    for (int i=0; i<n; i++) delete phils[i];
    return 0;
}

