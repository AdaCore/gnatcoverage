CC=gcc
CFLAGS=-fpreserve-control-flow -fdump-scos -g -save-temps
LDFLAGS=$(CFLAGS) -no-pie

driver: driver.o wibble.o

wibble.o: wibble.c wibble.h

driver.o: wibble.h driver.c

clean:
	rm -f *.o *.exe driver *.c.gli *.i *.s
