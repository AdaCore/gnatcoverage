# Simple makefile for building the zfp support library.

include Makefile.common

ifneq ($(TARGET),i386-linux)
  ifneq ($(TARGET),leon-elf)
  START=$(TARGET_SUPPORT_LIB)/start.o
  LDSCRIPT=$(TARGET_SUPPORT_LIB)/gnu.ld
  endif
  GPRCONFIGARGS=--target=$(TARGET) --config=Ada,,zfp
else
  GPRCONFIGARGS=--config=Ada
endif

all: $(TARGETCONFIG) $(START) $(LDSCRIPT) $(TARGET_SUPPORT_LIB)/libsupport.a

$(TARGETCONFIG):
	@$(MKDIR) $(TARGET_SUPPORT_LIB)
	gprconfig $(GPRCONFIGARGS) --config=C --config=Asm --batch -o $@

$(TARGET_SUPPORT_LIB)/start.o: src/$(TARGETNAME)/start.s
	$(CROSSPREFIX)gcc -c -o $@ $<

$(TARGET_SUPPORT_LIB)/gnu.ld: src/$(TARGETNAME)/gnu.ld
	$(CP) $< $@

$(TARGET_SUPPORT_LIB)/libsupport.a: force
	$(BUILDER) -XBUILD=yes -p -Psupport
	$(BUILDER) -XUNITTEST=yes -XBUILD=yes -p -Psupport

clean:
	$(RM) -rf lib obj

.PHONY: force clean
