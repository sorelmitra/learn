#ifndef _CHOPSTICKS_H_
#define _CHOPSTICKS_H_

class Chopsticks {
public:
    Chopsticks(int c) {
        count=c;
        sticks=new bool[count];
    }

    bool gain(int idx) throw(runtime_error) {
        check(idx);

        if (sticks[idx]) {
	   return false;
        }

        sticks[idx]=true;
        return true;
    }

    void release(int idx) throw(runtime_error) {
        check(idx);
        sticks[idx]=false;
    }

    virtual ~Chopsticks() {
        delete[] sticks;
        sticks=NULL;
    }

private:
    void check(int idx) throw(runtime_error) {
        if (idx < 0) {
	   throw runtime_error("idx too small");
        }
        if (idx >= count) {
	   throw runtime_error("idx too big");
        }
    }

private:
    int count;
    bool *sticks;
};

#endif // _CHOPSTICKS_H_
