#include <pthread.h>

#include <iostream>
#include <memory>

#include "CommunicatingThread.h"

using namespace std;

bool syncThread=true;

int s=0; // shared variable

pthread_mutex_t mutex=PTHREAD_MUTEX_INITIALIZER;

const int CommunicatingThread::count=10000000;

void *CommunicatingThread::run() {
    maybeLock(&mutex);
    cout<<"Thread "<<id<<" is running"<<endl;
    maybeUnlock(&mutex);
    for (int i=0; i<count; i++) {
        maybeLock(&mutex);
        s++;
        maybeUnlock(&mutex);
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
    unique_ptr<CommunicatingThread> t1(new CommunicatingThread(1, syncThread));
    unique_ptr<CommunicatingThread> t2(new CommunicatingThread(2, syncThread));
    t1->start();
    t2->start();
    t1->join();
    t2->join();
    cout<<"Threads have finished; s is "<<s<<endl;
    return 0;
}

