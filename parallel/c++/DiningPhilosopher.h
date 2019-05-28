#ifndef _DINING_PHILOSOPHER_H_
#define _DINING_PHILOSOPHER_H_

#include "lib/Thread.h"
#include "Chopsticks.h"

class DiningPhilosopher: public Thread {
public:
    DiningPhilosopher(Chopsticks *c, int l, int r, bool _sync)
        : chops(c), leftStick(l), rightStick(r) {
        sync=_sync;
        needLeftStick=false;
        needRightStick=false;
        if (count==0) {
	   srandom(100);
        }
        number=count++;
        cout << "I am phil " << number << " left stick " << leftStick <<
	   " right stick " << rightStick << endl;
    }
    virtual void *run();

private:
    std::string toString();
    void think();
    void eat();
    void logMsg(std::string s);

private:
    static unsigned int count;

    unsigned int number;
    Chopsticks *chops;
    int leftStick;
    int rightStick;
    bool needLeftStick;
    bool needRightStick;
};

#endif // _DINING_PHILOSOPHER_H_

