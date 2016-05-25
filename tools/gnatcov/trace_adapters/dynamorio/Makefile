include ../../architecture.mk
include ../../host.mk

DYNRIO_CONFIG=Release
DYNRIO_CLIENT=$(DYNRIO_CONFIG)/qtrace.dll

#########
# Build #  
#########

all: $(DYNRIO_CLIENT)

$(DYNRIO_CLIENT):
	cmake -DDynamoRIO_DIR=$(DYNRIO_PREFIX)/cmake .
	cmake --build . --config $(DYNRIO_CONFIG)

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
