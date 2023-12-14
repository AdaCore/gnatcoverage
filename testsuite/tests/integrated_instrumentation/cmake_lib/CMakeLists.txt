project(HelloWorld)
cmake_minimum_required(VERSION 3.0)

# Create a static library from the sources in src
add_library(my_lib STATIC src/lib.c)

# Create an executable using the library previously declared
add_executable(hello_world src/main.c)
target_link_libraries(hello_world my_lib)
