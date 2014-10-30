# Usage:
#
# cmake -DDynamoRIO_DIR=$PWD/../dynamorio/cmake ../dr-gnatcov
# make
# ../dynamorio/bin64/drrun -client libqtrace.so 0 -- /bin/ls

cmake_minimum_required(VERSION 2.6)

add_library(qtrace SHARED qtrace.c)
find_package(DynamoRIO)
if (NOT DynamoRIO_FOUND)
  message(FATAL_ERROR "DynamoRIO package required to build")
endif(NOT DynamoRIO_FOUND)

use_DynamoRIO_extension(qtrace drx) 

configure_DynamoRIO_client(qtrace)
