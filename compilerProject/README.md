# Gran Compiler

A compiler for the Gran programming language that supports both JIT compilation and C transpilation.

## Project Structure

```
.
├── include/         # Header files
│   └── lexer.h
├── src/            # Source files
│   ├── main.cpp
│   ├── lexer.cpp
│   └── transpiler.cpp
├── build/          # Build directory
├── tests/          # Test files
├── examples/       # Example Gran programs
└── CMakeLists.txt  # Build configuration
```

## Building the Project

```bash
# Create build directory
mkdir build && cd build

# Generate build files
cmake ..

# Build the project
make
```

## Running the Compiler

```bash
# JIT mode
./gran input.gran

# C transpilation mode
./granc input.gran
```

## Language Features

- Variable declarations
- Arithmetic and logical operators
- String comparison operators (===, ?=)
- Control structures (if, while, for)
- Functions with return values
- Comments (single-line and multiline)
- Input/Output operations

## Development Status

- [x] Lexer implementation
- [ ] Parser implementation
- [ ] LLVM IR generation
- [ ] JIT compilation
- [ ] C transpilation
- [ ] Command-line interface 