/*
 Sample program to run on the ARM device.
 */

/* Un-initialized variable; it will go into the '.bss' section */
int a;

/* Initialized variable; it goes to '.data' */
int b=0x12345678;

/*
 The default handler, referenced from startup.s.
 The CPU must always have instructions to execute, even if they're NOP.
 Because or program does nothing interesting, an infinite loop is a good
 implementation.
 */
void default_handler(void)
{
    while (1);
}

/* Constant variable that goes into '.rodata' */
const int c=20;

/*
 Our main function, called from startup.s.
 Again, our friend the infinite loop.
 */
main()
{
    while(1);
}

