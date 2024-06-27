main: main.o lib/libfoobar.so
	gcc -o main main.o -Llib/ -lfoobar

lib/libfoobar.so:
	$(MAKE) -C lib

%.o: %.c
	gcc -c $< -o $@	

clean:
	rm -rf main.o
	$(MAKE) -C lib clean
