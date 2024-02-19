main: main.o libbar/libbar.a libfoo/libfoo.a
	gcc -o main main.o -Llibbar/ -Llibfoo -lbar -lfoo

libbar/libbar.a:
	$(MAKE) -C libbar

libfoo/libfoo.a:
	$(MAKE) -C libfoo

%.o: %.c
	gcc -c $< -o $@

clean:
	rm -rf main.o
	$(MAKE) -C libbar clean
	$(MAKE) -C libfoo clean
