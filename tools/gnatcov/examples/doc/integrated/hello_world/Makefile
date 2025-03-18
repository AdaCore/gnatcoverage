CC=g++
OBJ = main.o

%.o: %.c
	$(CC) -c -o $@ $<

test: $(OBJ)
	$(CC) -o $@ $^
