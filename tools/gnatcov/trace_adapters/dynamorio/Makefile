include ../../architecture.mk
include ../../host.mk


DYNRIO_CONFIG=Release
DYNRIO_CLIENT=INVALID

ifeq ($(HOST_OS), windows)
DYNRIO_CLIENT=build/$(DYNRIO_CONFIG)/qtrace.dll
G_OPTION=-G"Visual Studio 17 2022"
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
	cd build && cmake -Wno-dev -DDynamoRIO_DIR=$(DYNRIO_PREFIX)/cmake $(G_OPTION) ..
	cd build && cmake --build . --config $(DYNRIO_CONFIG) -v 

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
