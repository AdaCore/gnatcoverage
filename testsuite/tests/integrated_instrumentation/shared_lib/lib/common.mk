ifeq ($(OS),Windows_NT)
	LIB := libfoobar.dll
else
	LIB := libfoobar.so
endif
