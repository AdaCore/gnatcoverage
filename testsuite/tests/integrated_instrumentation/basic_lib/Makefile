main: main.o lib/libfoobar.a
	gcc -o main main.o -Llib/ -lfoobar

lib/libfoobar.a:
	$(MAKE) -C lib

%.o: %.c
	gcc -c $< -o $@	

clean:
	rm -rf main.o
	$(MAKE) -C lib clean
