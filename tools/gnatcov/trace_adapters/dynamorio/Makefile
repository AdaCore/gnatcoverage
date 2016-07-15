include ../../architecture.mk
include ../../host.mk


DYNRIO_CONFIG=Release
DYNRIO_CLIENT=INVALID

ifeq ($(HOST_OS), windows)
DYNRIO_CLIENT=build/$(DYNRIO_CONFIG)/qtrace.dll
ifeq ($(BITS_ARCH), 32)
G_OPTION=-G"Visual Studio 12"
else
G_OPTION=-G"Visual Studio 12 Win64"
endif
endif

ifeq ($(HOST_OS), linux)
DYNRIO_CLIENT=build/libqtrace.so
G_OPTION=
endif

#########
# Build #  
#########

all: $(DYNRIO_CLIENT)

.PHONY: $(DYNRIO_CLIENT)

$(DYNRIO_CLIENT):
	mkdir build || true
	cd build && cmake -DDynamoRIO_DIR=$(DYNRIO_PREFIX)/cmake $(G_OPTION) ..
	cd build && cmake --build . --config $(DYNRIO_CONFIG)

################
# Installation #
################

.PHONY: install

install:
	mkdir -p $(DYNRIO_CLIENT_INSTALL_DIR)
	cp -p $(DYNRIO_CLIENT) $(DYNRIO_CLIENT_INSTALL_DIR)

###########
# Cleanup #
###########

.PHONY: clean

clean:
	rm -f $(DYNRIO_CLIENT)
