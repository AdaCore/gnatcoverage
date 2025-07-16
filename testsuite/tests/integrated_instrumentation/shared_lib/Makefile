include lib/common.mk

main: main.o lib/$(LIB)
	gcc -o main main.o -Llib/ -lfoobar

lib/$(LIB):
	$(MAKE) -C lib

%.o: %.c
	gcc -c $< -o $@	

clean:
	rm -rf main.o
	$(MAKE) -C lib clean
