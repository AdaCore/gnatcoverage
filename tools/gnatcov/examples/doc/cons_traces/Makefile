all:
	gnatcov instrument -P example1 --dump-filename-simple
	gprbuild -P example1 --implicit-with=gnatcov_rts --src-subdirs=gnatcov-instr
	./obj/test_cmd_safe
	./obj/test_cmd_unsafe
	gnatcov coverage -P example1 --level=stmt --annotate=xcov --output-dir=safe-xcov test_cmd_safe.srctrace
	gnatcov coverage -P example1 --level=stmt --annotate=xcov --output-dir=unsafe-xcov test_cmd_unsafe.srctrace
	gnatcov coverage -P example1 --level=stmt --annotate=xcov --output-dir=combine-xcov test_cmd_safe.srctrace test_cmd_unsafe.srctrace
