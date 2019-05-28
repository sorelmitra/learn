TARGET_PREFIX=arm-none-eabi

test-arm.bin : test-arm.elf
	$(TARGET_PREFIX)-objcopy -strip --target=elf32-littlearm -O binary test-arm.elf test-arm.bin

test-arm.elf : test-arm.o startup.o test.ld
	$(TARGET_PREFIX)-ld -static -Ttest.ld -o test-arm.elf test-arm.o startup.o

test-arm.o : test-arm.c
	$(TARGET_PREFIX)-gcc -g -c test-arm.c -mthumb -mcpu=cortex-m3

startup.o : startup.s
	$(TARGET_PREFIX)-as -mthumb -mcpu=cortex-m3 -globalize-symbols startup.s -o startup.o

clean :
	rm -rf *~ ; rm *.o ; rm *.elf ; rm *.bin
