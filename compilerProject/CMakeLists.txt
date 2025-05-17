cmake_minimum_required(VERSION 3.10)
project(GranCompiler VERSION 1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(${PROJECT_SOURCE_DIR}/include)

# Source files
set(SOURCES
    src/main.cpp
    src/lexer.cpp
    src/transpiler.cpp
)

# Create executable
add_executable(gran ${SOURCES})

# Add compiler flags
target_compile_options(gran PRIVATE -Wall -Wextra)

# Create build directory
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/build) 