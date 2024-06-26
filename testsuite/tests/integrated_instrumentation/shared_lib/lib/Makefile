libfoobar.so: foo.o bar.o
	gcc -shared -o $@ $^

%.o: %.c
	gcc -c -fpic $< -o $@

clean:
	rm -f foo.o bar.o libfoobar.so
