#include <iostream>
#include <stdio.h>
#include <unistd.h>

using namespace std;

class X {
    int i;
public:
    X(int ii = 0);
    void modify();
};

X::X(int ii) { i = ii; }

void X::modify() { i++; }

X f5() {
    return X();
}

const X f6() {
    return X();
}

void f7(X& x) { // Pass by non-const reference
    x.modify();
}

class Fred {
  const int size;
public:
  Fred(int sz);
  void print();
};

Fred::Fred(int sz)
  : size(sz)
{
    // size = sz; // Generates "uninitialized member" error
}
void Fred::print() { cout << size << endl; }

class A
{
public:
    A(int value) : dummy(value) {}
        
    friend ostream& operator<<(ostream &os, const A &a) 
    {
        os << "Hi, this is A, " << a.dummy;
    }

    A& operator=(const A &a) 
    {
        dummy = a.dummy * 2;
    }
    
private:
    int dummy;
};


int main() {
    cout << "Hello, "
        "world!" << endl;

    printf("The page size for this system is %ld bytes.\n",
               sysconf(_SC_PAGESIZE)); /* _SC_PAGE_SIZE is OK too. */
    X x = f5();
    f7(x);

    Fred(3).print();

    A a(2);
    A b(3);
    cout << a << "; " << b << endl;
    b = a;
    cout << b << endl;
}
