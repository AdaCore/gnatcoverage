libbar.a: foo.o bar.o
	ar -rc $@ $^

%.o: %.c
	gcc -c $< -o $@

clean:
	rm -f foo.o bar.o libbar.a
