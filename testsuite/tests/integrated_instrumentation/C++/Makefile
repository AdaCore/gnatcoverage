CC=g++
OBJ = test.o

%.o: %.cpp
	$(CC) -c -o $@ $<

test: $(OBJ)
	$(CC) -o $@ $^

clean:
	rm -f *.o test
