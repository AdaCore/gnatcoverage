CC=g++
OBJ = pkg.o test.o

%.o: %.c
	$(CC) -c -pedantic -Wall -o $@ $<

test: $(OBJ)
	$(CC) -pedantic -o $@ $^

clean:
	rm -f *.o test
