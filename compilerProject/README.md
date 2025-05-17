# Gran Programming Language Compiler

A simple programming language compiler built using LLVM, featuring a clean syntax and basic programming constructs.

## Prerequisites

Before you begin, ensure you have the following installed:

1. **LLVM Development Tools**
   ```bash
   # For Ubuntu/Debian
   sudo apt-get install llvm-19-dev clang-19
   
   # For Fedora
   sudo dnf install llvm-devel clang
   
   # For macOS
   brew install llvm
   ```

2. **C++ Compiler**
   ```bash
   # For Ubuntu/Debian
   sudo apt-get install g++-12
   
   # For Fedora
   sudo dnf install gcc-c++
   
   # For macOS
   xcode-select --install
   ```

3. **Build Tools**
   ```bash
   # For Ubuntu/Debian
   sudo apt-get install make cmake
   
   # For Fedora
   sudo dnf install make cmake
   
   # For macOS
   brew install make cmake
   ```

## Installation

1. **Clone the Repository**
   ```bash
   git clone https://github.com/yourusername/gran-compiler.git
   cd gran-compiler
   ```

2. **Build the Project**
   ```bash
   # Clean any previous builds
   make clean
   
   # Build the compiler and runtime
   make
   ```

3. **Verify Installation**
   ```bash
   # Run the test program
   LD_LIBRARY_PATH=. ./gran test.gran
   ```

## Project Overview

Gran is a statically-typed programming language with a focus on simplicity and readability. The compiler is built using LLVM for code generation and optimization.

### Current Features
- Basic arithmetic operations (+, -, *, /)
- Variable declarations and assignments
- Print statements (screenit)
- Integer and string literals
- Basic control flow (if-else, while loops)
- Function declarations and calls

### Language Syntax
```gran
// Variable declaration and initialization
var x = 42;
var y = 10 + 5;

// Print statements
screenit "Hello from Gran!";
screenit x;
screenit y;

// Arithmetic operations
var sum = x + y;
screenit sum;

// Control flow
if (x > y) {
    screenit "x is greater";
} else {
    screenit "y is greater";
}

// While loops
var i = 0;
while (i < 5) {
    screenit i;
    i = i + 1;
}
```

## Project Structure
```
.
├── include/           # Header files
│   ├── lexer.h       # Lexical analyzer
│   ├── parser.h      # Parser
│   └── ir_generator.h # LLVM IR generator
├── src/              # Source files
│   ├── main.cpp      # Main compiler driver
│   ├── lexer.cpp     # Lexer implementation
│   ├── parser.cpp    # Parser implementation
│   └── ir_generator.cpp # IR generator implementation
├── runtime.c         # Runtime library
├── test.gran        # Example program
└── Makefile         # Build configuration
```

## Development Setup

### Setting Up Your Development Environment

1. **IDE Setup**
   - Recommended: Visual Studio Code with C/C++ extensions
   - Alternative: CLion, Eclipse CDT, or any C++ IDE

2. **LLVM Configuration**
   - Ensure LLVM is in your system PATH
   - Set up LLVM environment variables:
     ```bash
     export LLVM_HOME=/usr/lib/llvm-19
     export PATH=$LLVM_HOME/bin:$PATH
     export LD_LIBRARY_PATH=$LLVM_HOME/lib:$LD_LIBRARY_PATH
     ```

3. **Build Configuration**
   - The project uses Make for building
   - Main build targets:
     - `make` - Build everything
     - `make clean` - Clean build artifacts
     - `make test` - Run tests (when implemented)

### Running the Compiler

1. **Basic Usage**
   ```bash
   # Method 1: Using rpath (recommended)
   ./gran your_program.gran
   
   # Method 2: Using LD_LIBRARY_PATH
   LD_LIBRARY_PATH=. ./gran your_program.gran
   ```

2. **Debug Mode**
   ```bash
   # Using rpath
   ./gran -d your_program.gran
   
   # Using LD_LIBRARY_PATH
   LD_LIBRARY_PATH=. ./gran -d your_program.gran
   ```

3. **Example Programs**
   ```bash
   # Using rpath
   ./gran test.gran
   ./gran path/to/your/program.gran
   
   # Using LD_LIBRARY_PATH
   LD_LIBRARY_PATH=. ./gran test.gran
   LD_LIBRARY_PATH=. ./gran path/to/your/program.gran
   ```

### Writing Gran Programs

1. **Basic Syntax**
   ```gran
   // Variable declaration
   var x = 42;
   
   // Print statement
   screenit "Hello, World!";
   
   // Arithmetic
   var y = x + 10;
   screenit y;
   ```

2. **Control Flow**
   ```gran
   // If statement
   if (x > 0) {
       screenit "Positive";
   } else {
       screenit "Negative";
   }
   
   // While loop
   var i = 0;
   while (i < 5) {
       screenit i;
       i = i + 1;
   }
   ```

## Challenges Faced

1. **Runtime Integration**
   - Initially struggled with proper type handling in the runtime library
   - Fixed by creating separate functions for different types (screenit, screenit_int, screenit_double)
   - Challenge: Ensuring proper type safety between LLVM IR and C runtime

2. **LLVM IR Generation**
   - Complex handling of variable scoping and memory management
   - Proper loading of variable values before operations
   - Challenge: Understanding LLVM's type system and memory model

3. **Build System**
   - Issues with shared library compilation and linking
   - Fixed by properly configuring Makefile for both C and C++ compilation
   - Challenge: Managing dependencies between runtime and compiler

4. **Type System**
   - Ensuring proper type checking and conversion
   - Handling different types in print statements
   - Challenge: Maintaining type safety across the compiler pipeline

## Team Distribution

### Member 1: Lexer and Parser
**Responsibilities:**
- Implement lexical analysis
- Design and implement parser
- Handle syntax error reporting
- Study:
  - Regular expressions and finite automata
  - Context-free grammars
  - Recursive descent parsing
  - Error recovery techniques

### Member 2: IR Generator
**Responsibilities:**
- LLVM IR generation
- Type system implementation
- Code optimization
- Study:
  - LLVM IR and its structure
  - Static Single Assignment (SSA) form
  - Basic block and control flow
  - LLVM optimization passes

### Member 3: Runtime and Build System
**Responsibilities:**
- Runtime library implementation
- Build system configuration
- Testing framework
- Study:
  - C/C++ linking and loading
  - Shared library development
  - Make and CMake
  - Unit testing frameworks

### Member 4: Language Design and Documentation
**Responsibilities:**
- Language specification
- Documentation
- Example programs
- Study:
  - Programming language design principles
  - Technical writing
  - Markdown and documentation tools
  - Language feature implementation

## Future Work

1. **Language Features**
   - [ ] Arrays and structures
   - [ ] Classes and objects
   - [ ] Exception handling
   - [ ] Standard library
   - [ ] Import/export system

2. **Compiler Improvements**
   - [ ] Better error messages
   - [ ] More optimizations
   - [ ] Debug information
   - [ ] Cross-platform support
   - [ ] Better type inference

3. **Tooling**
   - [ ] Language server
   - [ ] IDE integration
   - [ ] Package manager
   - [ ] Testing framework
   - [ ] Documentation generator

4. **Runtime**
   - [ ] Garbage collection
   - [ ] Concurrency support
   - [ ] Better memory management
   - [ ] Standard library expansion
   - [ ] Performance profiling

## Troubleshooting

1. **Common Issues**

   a. **LLVM Not Found**
   ```bash
   # Error: llvm-config not found
   # Solution: Install LLVM development package
   sudo apt-get install llvm-19-dev
   ```

   b. **Runtime Library Not Found**
   ```bash
   # Error: libruntime.so not found
   # Solution 1: Use LD_LIBRARY_PATH
   export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
   
   # Solution 2: Rebuild with rpath (recommended)
   make clean
   make
   ```

   c. **Build Errors**
   ```bash
   # Clean and rebuild
   make clean
   make
   ```

2. **Debugging Tips**
   - Use `-d` flag for debug output
   - Check compiler logs in case of errors
   - Verify LLVM installation and paths

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details. 