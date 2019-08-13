#include <pthread.h>

#include "Thread.h"

Thread::Thread() throw(runtime_error) {
    sync=true;
    checkResult(pthread_attr_init(&threadAttr));
}

void Thread::start() throw(runtime_error) {
    checkResult(pthread_create(
        &threadId, &threadAttr, &sStartDispatcher, this));
}

void *Thread::join() throw(runtime_error) {
    checkResult(pthread_join(threadId, NULL));
    return result;
}

Thread::~Thread() throw(runtime_error) {
    checkResult(pthread_attr_destroy(&threadAttr));
}

int Thread::maybeLock(pthread_mutex_t *mutex) {
    if (!sync) return 0;
    return pthread_mutex_lock(mutex);
}

int Thread::maybeUnlock(pthread_mutex_t *mutex) {
    if (!sync) return 0;
    return pthread_mutex_unlock(mutex);
}

void *Thread::sStartDispatcher(void *arg) throw(runtime_error) {
    Thread *t=static_cast<Thread *>(arg);
    if (t==NULL) {
        throw runtime_error("NULL argument");
    }
    t->result=t->run();
    return t->result;
}

void Thread::checkResult(int result) throw(runtime_error) {
    if (result==0) {
        return;
    }
    throw runtime_error(strerror(result));
}

