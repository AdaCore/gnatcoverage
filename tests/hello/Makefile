QEMU=qemu-system-ppc
XCOV=../../tools/xcov/xcov
CROSS=powerpc-elf-
CFLAGS=-g -O
support_dir=../support/src/ppc-elf

all: hello.adb.xcov

hello.adb.xcov: hello.trace
	$(XCOV) -r $< -e hello --source-coverage

hello.trace: hello.bin
	$(QEMU) -nographic -M prep -L . -bios $< -trace $@ /dev/null

hello.bin: hello
	$(CROSS)objcopy -O binary $< $@

hello: start.o c_io.o force
	$(CROSS)gnatmake $(CFLAGS) hello -I$(support_dir) -largs start.o c_io.o -Wl,-T$(support_dir)/ppc.ld -nostdlib

start.o: $(support_dir)/start.s
	$(CROSS)gcc -c -o $@ $<

c_io.o: $(support_dir)/c_io.c
	$(CROSS)gcc -c $(CFLAGS) -o $@ $<

clean:
	$(RM) -f *.o *.ali *.bin *.trace b~* *~ *.xcov hello

.PHONY: force clean