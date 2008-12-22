QEMU=qemu-system-ppc
XCOV=xcov
CFLAGS=-g -O1 -fpreserve-control-flow

TARGET = powerpc-elf

BASE_SUPPORT_DIR = ../support
TARGET_SUPPORT_DIR = $(BASE_SUPPORT_DIR)/$(TARGET)

SUPPORT_DIRS=$(BASE_SUPPORT_DIR) $(TARGET_SUPPORT_DIR)

TESTS = 

all: $(TESTS:%=%.adb.xcov)

CROSS = $(TARGET)-

%.adb.xcov: %.trace
	$(XCOV) -r $< -e $(basename $<) \
	 --output-format=html --asm --source-coverage

%.trace: %.bin
	$(QEMU) -nographic -M prep -L . -bios $< -trace $@ /dev/null

%.bin: %
	$(CROSS)objcopy -O binary $< $@

$(TESTS): start.o c_io.o force
	$(CROSS)gnatmake $(CFLAGS) $@ $(SUPPORT_DIRS:%=-I%) \
	-largs start.o c_io.o -Wl,-T$(TARGET_SUPPORT_DIR)/gnu.ld -nostdlib

start.o: $(TARGET_SUPPORT_DIR)/start.s
	$(CROSS)gcc -c -o $@ $<

c_io.o: $(TARGET_SUPPORT_DIR)/c_io.c
	$(CROSS)gcc -c $(CFLAGS) -o $@ $<

clean:
	$(RM) -f *.o *.ali *.bin *.trace b~* *~ *.xcov $(TESTS)

.PHONY: force clean