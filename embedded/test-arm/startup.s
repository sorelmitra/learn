@ This is assembler for ARM CPUs

@ The purpose of this script is to initialize the device so that our C program
@ can run.

.syntax unified

@ Define the 'vectors' section that is reffered in the 'test.ld' linker script
.section "vectors"

@ Jump at the starting point.
@ The processor needs to execute something, and as we defined it in the linker
@ script, it will start with our '.text' section, which points right here.
@ We tell it to jump to our code that initializes the system and then calls 
@ the main function of our C program.
reset: b _start         @ start point

@ Immediately after the jumping instruction we define many machine words.
@ Each word is a pointer to a symbol that represents a handler for a certain
@ event. Each event has a fixed place wher it can appear in this list.
@ Here we create a default handler for most of them.
.word default_handler   @ NMI
.word default_handler   @ hard-fault
.word default_handler   @ mem-manage
.word default_handler   @ bus-fault
.word default_handler   @ usage-fault
.word 0
.word 0
.word 0
.word 0
.word default_handler   @ SVC-handler
.word default_handler   @ debug-monitor
.word 0
.word default_handler   @ pend-SV
.word default_handler   @ sys-tick
.word default_handler   @ IRQ

@ This is where the 'b' jump goes.
@ Here starts our "operating system", which does little now:
@ - Initialize the stack pointer in order for the C program to work
@ - Copy '.data' to RAM
@ - Initialize to 0 '.bss'
@ - Jump to the main function of the C program

_start:
    ldr sp, =0x20004000 @ load register 'sp' with the last addr of our board
		      @ 'sp' is the stack pointer

    @ Prepare to copy '.data' to RAM. Store in registers values stored in the
    @ symbols defined in the 'test.ld' linker script.
    ldr r0, =flash_data
    ldr r1, =ram_sdata
    ldr r2, =data_size
    cmp r2, #0           @ If '.data' length is 0
    beq init_bss         @ Then skip copying to RAM

@ Loop that copies from r0 (flash_data) to r1 (ram_data)
copy_data:
    ldrb r4, [r0], #1    @ Load a byte from the address pointed by r0 to r4,
		       @ and increment r0 by 1
    strb r4, [r1], #1    @ Store the byte from r4 to the address pointed by r1,
		       @ and increment r1 by 1
    subs r2, r2, #1
    bne copy_data

@ Prepare initializing '.bss' to zero
init_bss:
    ldr r2, =bss_size
    cmp r2, #0
    @ Don't initialize if length of '.bss' is 0
    beq main

    mov r0, #0
    ldr r1, =ebss
    ldr r3, =sbss

@ Loop to initialize '.bss' to 0, starting from the end
setzero:
    strb r0, [r1], #-1
    subs r2, r2, #1
    bne setzero

    @ Jump to main
    b main    

