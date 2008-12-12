QEMU=qemu-system-ppc
XCOV=../../tools/xcov/xcov
CROSS=powerpc-elf-
CFLAGS=-g -O0
ppc_support_dir=../support/src/ppc-elf
zfp_support_dir=../../../board-support/zfp-support

TNAME=explore
all: $(TNAME).adb.xcov

$(TNAME).adb.xcov: $(TNAME).trace
	$(XCOV) -r $< -e $(TNAME) --output-format=html --source-coverage

$(TNAME).trace: $(TNAME).bin
	$(QEMU) -nographic -M prep -L . -bios $< -trace $@ /dev/null

$(TNAME).bin: $(TNAME)
	$(CROSS)objcopy -O binary $< $@

$(TNAME): start.o c_io.o force
	$(CROSS)gnatmake $(CFLAGS) -gnatp -a $(TNAME) \
	-I$(ppc_support_dir) -I$(zfp_support_dir) \
	-largs start.o c_io.o -Wl,-T$(support_dir)/ppc.ld -nostdlib

start.o: $(ppc_support_dir)/start.s
	$(CROSS)gcc -c -o $@ $<

c_io.o: $(ppc_support_dir)/c_io.c
	$(CROSS)gcc -c $(CFLAGS) -o $@ $<

clean:
	$(RM) -f *.html *.o *.ali *.bin *.trace b~* *~ *.xcov $(TNAME)

.PHONY: force clean
