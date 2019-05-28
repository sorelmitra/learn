Short writeup about how to prepare an environment for working with a simulator for a board, and writing programs for it.

The board in question is the lm3s6965 evaluation board.

Install Environment in Ubuntu

Install the QEmu system emulator and the arm-linux-gnueabi target binutils applications (except GDB):

apt-get install qemu-system gcc-4.6-arm-linux-gnueabi binutils-doc

Install the arm-linux-gnueabi GDB:

apt-get install libncurses5-dev ncurses-dev # for building GDB

Get the latest GDB source code, extract it and cd to it. Configure it for the arm-linux-gnueabi target, make and install it:

./configure --target=arm-linux-gnueabi
make
sudo make install

Start QEmu for Your Board

qemu-system-arm -cpu cortex-m3 -M lm3s6965evb -S -gdb tcp::1234 -nographic -kernel <my-elf-file>

Build a Simple Elf File with a Minimal C Program

First of all, you need to create a linker script to define how data is organized. This is explained in the sample 'test.ld' script.

Then you need to create a startup code, written in assembler, that initializes the stack, the initialized data, optionally the non-initialized data, and then jumps to the main function in your C program. This is explained in the sample 'startup.s' assembly code file.

Then you can create a C program that runs on your device. A sample explained program is in 'test-arm.c'.

Run GDB/DDD

To debug your program, run GDB like this:

arm-none-eabi-gdb test-arm.elf

In GDB, run this command:

source commands.gdb

This will set the target remote and load the symbols.

Then you can continue and use GDB/DDD as usual.

