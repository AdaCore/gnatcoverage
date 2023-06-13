CC=gcc
AR=ar
CFLAGS=-fpreserve-control-flow -fdump-scos -g -save-temps
LDFLAGS=$(CFLAGS) -no-pie

all: libdriver.a

libdriver.a: driver.o wibble.o
	$(AR) cru libdriver.a driver.o wibble.o

wibble.o: libsrc/wibble.c libsrc/wibble.h
	$(CC) -o $@ -c $< $(CFLAGS)
driver.o: libsrc/driver.c libsrc/wibble.h
	$(CC) -o $@ -c $< $(CFLAGS)
