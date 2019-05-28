/*
 * This is an Objective-C sample file that displays a number on Linux.
 *
 * Compile with:
 * gcc -o a a.m -lobjc
 */

#import <objc/Object.h>
#import <stdio.h>

@interface Number: Object
{ // Accolades after the @interface keyword contain the data members
@private
    int number;
}

// The interface declaration continues with member functions declaration, until @end is reached

- (int) num;
- (void) setNumWithInt:(int) num;

- (void) setNumAddNumber:(int) num1 withNumber:(int) num2; // The method name includes its parameters.
                                                           // The first parameter name is included in
                                                           // the method name.
- (void) printNum;

@end

@implementation Number: Object

// The @implementation declaration contains the implementation for the corresponding @interface.
// Similarly, it continues until @end is reached.

- (int) num
{
    return self->number; // 'self' is how you access the current object (equivalent to 'this' in C++)
}

- (void) setNumWithInt:(int) num
{
    number = num;
}

- (void) setNumAddNumber:(int) num1 withNumber:(int) num2
{
    number = num1 + num2;
}

- (void)printNum
{
    printf("%d\n", number);
}

@end

int main(void)
{
    Number *myNumber = [Number new]; // equal to [[Number alloc] init]
    [myNumber setNumWithInt:5];
    [myNumber printNum];
    [myNumber setNumAddNumber:6 withNumber:[myNumber num]];
    [myNumber printNum];
    return 0;
}
