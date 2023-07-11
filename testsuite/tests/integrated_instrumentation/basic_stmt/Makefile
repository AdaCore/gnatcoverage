CC=gcc
OBJ = pkg.o test.o

%.o: %.c
	$(CC) -c -o $@ $<

test: $(OBJ)
	$(CC) -o $@ $^

clean:
	rm -f *.o test
