TESTS=test_services
XCOVLEVEL=$(XCOVLEVEL)
include ../support/Makefile.examples

all: index-test_services.xml report-test_services.txt
	./xcov-report.py index-test_services.xml
