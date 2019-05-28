SECTIONS {
    /* 
     '.' is the current output location counter.
     We assign it to 0, which is the start address of the flash for this CPU.

     The purpose here is to copy the parts of the program to the flash.

     The flash is holding the non-volatile data. Upon initialization of the
     board, the RAM is empty, but the flash retains its content. It is the
     responsability of the startup script (e.g. startup.s hereby) to copy from
     the flash to the RAM the data and the code.

     This script defines some symbols to be accessed from the assembly code 
     that is responsible for copying from flash to RAM (startup.s).
     */
    . = 0x00000000;

    /*
     Define a '.text' section placed at '.'.

     This copies the 'vectors' section from all object files into the '.text'
     section of the linker's output file (in this case test-arm.elf).
     In our case, only 'startup.s' contains a section named vectors, so this
     '.text' section will contain the assembly code that starts up the device.

     Because '.' is at 0x0 address, this '.text' section is placed there (at
     the beginning of the flash).
     */
    .text : { * (vectors); }

    /*
     This is not used right now, but is a symbol available to the C program.
     It measures the size of the 'vectors' section, i.e. of the assembly 
     startup code.

     '.' is updated to point after the end of the previous section.
     Each section assignment updates '.'.
     */
    vectors_size = .;

    /*
     Define another '.text' section to include all '.text' sections from all 
     object files. The C compiler produces '.text' sections for the source
     code of your program.
     */
    .text : { * (.text); }

    /*
     A section for the constants. There's no reason to move it in RAM.
     */
    .rodata : { * (.rodata); }

    /*
     Hold the end of the flash data in this symbol.
     */
    flash_data = .;

    /*
     Now moving to the first address of the RAM.
     */
    . = 0x20000000;

    /*
     Retain the start of the .data section in RAM.
     */
    ram_sdata = .;

    /*
     The '.data' section. Up until here, the sections were held only in flash.
     From now on, the sections must be held both in the flash and RAM, but at
     different moments of the application's lifecycle.

     All parts of the program must be stored in the flash, because the RAM 
     is volatile. But the program needs to run in the RAM, so at least the
     non-constant data must be moved in the RAM.

     The sections defined until here were referring to the run-time storage
     - which happened to be the same as with the non-volatile storage.
     But here we need to define a section to be in the flash, and also copied
     to the RAM.
     To achieve this, we use the "AT" keyword, which instructs the linker to
     do two things:
     - Store the data from the section AT the given address, instead of 
       storing it at '.'
     - Increment '.' as usual with all section definitions, thus leaving the
       corresponding place in RAM 'empty', but marking it as 'occupied'. It
       effectively reserves space in the RAM for the section, for it to be
       copied later.

     In our case, we store the '.data' section at flash_data, which points 
     immediately after the last section that we defined in the flash above.
     After that, '.' is incremented with the size of '.data'.

     The copying to RAM is done from startup.s. For this purpose we create the
     ram_sdata and data_size symbols to hold the start address and length of 
     the data section. These symbols are used in startup.s to copy the data
     to RAM.
     */
    .data : AT (flash_data) { * (.data); }
    ram_edata = .;
    data_size = (ram_edata - ram_sdata);

    /*
     Now we do the same thing for the '.bss' data, but since this stores the
     un-initialized variables, there's no need to store it in the flash, so
     we put it directly in RAM.

     We still define the symbols with the start address and the size, but this
     time in order to initialize the whole .bss block with 0 (which is good
     practice).
     */
    sbss = .;
    .bss : { * (.bss); }
    ebss = .;
    bss_size = ebss -sbss;
}

