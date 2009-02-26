# Simple makefile for building the zfp support library.

include Makefile.common

ifneq ($(TARGET),)
  START=$(TARGET_SUPPORT_LIB)/start.o
  LDSCRIPT=$(TARGET_SUPPORT_LIB)/gnu.ld
  GPRCONFIGARGS=--target=$(TARGET)
endif

all: $(TARGET_SUPPORT_LIB)/libsupport.a $(START) $(LDSCRIPT)

$(TARGETCONFIG):
	@$(MKDIR) $(TARGET_SUPPORT_LIB)
	gprconfig $(GPRCONFIGARGS) --config=Ada,,zfp --config=C --batch -o $@

$(TARGET_SUPPORT_LIB)/libsupport.a: $(TARGETCONFIG) force
	$(BUILDER) -p -Psupport_build.gpr

$(TARGET_SUPPORT_LIB)/start.o: src/$(TARGETNAME)/start.s
	$(CROSS)gcc -c -o $@ $<

$(TARGET_SUPPORT_LIB)/gnu.ld: src/$(TARGETNAME)/gnu.ld
	$(CP) $< $@

clean:
	$(RM) -rf lib obj

.PHONY: force clean
