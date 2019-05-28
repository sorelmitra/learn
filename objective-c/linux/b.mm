/*
 * This is an Objective-C++ sample file that displays two numbers on Linux.
 * It demonstrates intermixing of Objective-C and C++ code freely.
 *
 * Compile with:
 * g++ -o b b.mm -lobjc
 */

#import <objc/Object.h>
#import <stdio.h>

@interface Number: Object
{
@public
    int number;
}

- (void)printNum;

@end

@implementation Number: Object

- (void)printNum
{
    printf("%d\n", number);
}

@end

class CppNumber {
    private:
        int number;

    public:
        CppNumber(void) {
            number = 0;
        }
        CppNumber(Number *nr) {
            number = nr->number + 1; // Call a member of an Objective-C class as it would be a C++ class
        }
        void printNumber(void) {
            printf("C++ number is %d\n", number);
        }
};

int main(void)
{
    Number *myNumber = [Number new]; // equal to [[Number alloc] init]
    myNumber->number = 6;
    [myNumber printNum];
    CppNumber *cppNumber = new CppNumber(myNumber); // Call C++ allocator and constructor with an Objective-C parameter
    cppNumber->printNumber(); // Member of a C++ class is called in C++ manner
    return 0;
}
