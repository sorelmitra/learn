#ifndef _COMMUNICATING_THREAD_H_
#define _COMMUNICATING_THREAD_H_

#include "lib/Thread.h"

class CommunicatingThread: public Thread {
public:
    CommunicatingThread(int _id, bool _sync): id(_id) {
        sync=_sync;
    }
    virtual void *run();
private:
    static const int count;
    int id;
};

#endif // _COMMUNICATING_THREAD_H_

