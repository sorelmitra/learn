#ifndef _THREAD_H_
#define _THREAD_H_

#include <pthread.h>
#include <string.h>

#include <stdexcept>

using namespace std;

class Thread {
public:
    Thread() throw(runtime_error);
    void start() throw(runtime_error);
    virtual void *run()=0;
    void *join() throw(runtime_error);
    virtual ~Thread() throw(runtime_error);

protected:
    int maybeLock(pthread_mutex_t *mutex);
    int maybeUnlock(pthread_mutex_t *mutex);

protected:
    bool sync;

private:
    static void *sStartDispatcher(void *arg) throw(runtime_error);

    void checkResult(int result) throw(runtime_error);

private:
    pthread_attr_t threadAttr;
    pthread_t threadId;
    void *result;
};

#endif // _THREAD_H_

