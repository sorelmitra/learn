#!/bin/bash
qemu-system-arm -cpu cortex-m3 -M lm3s6965evb -S -gdb tcp::1234 -nographic -kernel test-arm.elf
