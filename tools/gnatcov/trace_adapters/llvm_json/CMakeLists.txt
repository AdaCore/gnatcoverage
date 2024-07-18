cmake_minimum_required(VERSION 3.0)
project(gnatcov_llvm_exporter)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(CMakePrintHelpers)

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Set LLVM include directories
include_directories(${LLVM_INCLUDE_DIRS})

# Set LLVM definitions
add_definitions(${LLVM_DEFINITIONS})
add_definitions("-fno-rtti")

# Set source files
set(SOURCE_FILES src/main.cpp)

# Add executable target
add_executable(gnatcov_llvm_exporter ${SOURCE_FILES})

# Link LLVM libraries
llvm_map_components_to_libnames(
    LLVM_LIBRARIES
    demangle
    coverage
    profiledata
    support
)

# Link LLVM libraries to the executable
target_link_libraries(gnatcov_llvm_exporter ${LLVM_LIBRARIES})
