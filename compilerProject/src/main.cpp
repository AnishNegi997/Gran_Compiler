#include <dlfcn.h>
#include <iostream>
#include <fstream>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/CodeGen.h>
#include <filesystem>
#include "../include/lexer.h"
#include "../include/parser.h"
#include "../include/ir_generator.h"

// Function pointer types
typedef void (*ScreenitFunc)(const char*);
typedef void (*ScreenitIntFunc)(int);
typedef void (*ScreenitDoubleFunc)(double);

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <source_file>" << std::endl;
        return 1;
    }

    std::cerr << "Starting compilation..." << std::endl;

    // Get absolute path to libruntime.so
    std::filesystem::path runtime_path = std::filesystem::absolute("libruntime.so");
    if (!std::filesystem::exists(runtime_path)) {
        std::cerr << "Error: libruntime.so not found at " << runtime_path << std::endl;
        return 1;
    }
    std::cerr << "Found libruntime.so at: " << runtime_path << std::endl;

    // Load runtime library
    void* handle = dlopen(runtime_path.c_str(), RTLD_LAZY);
    if (!handle) {
        std::cerr << "Failed to load runtime library: " << dlerror() << std::endl;
        return 1;
    }
    std::cerr << "Loaded libruntime.so successfully" << std::endl;

    // Get function pointers
    ScreenitFunc screenit = (ScreenitFunc)dlsym(handle, "screenit");
    ScreenitIntFunc screenit_int = (ScreenitIntFunc)dlsym(handle, "screenit_int");
    ScreenitDoubleFunc screenit_double = (ScreenitDoubleFunc)dlsym(handle, "screenit_double");

    if (!screenit || !screenit_int || !screenit_double) {
        std::cerr << "Failed to load screenit functions: " << dlerror() << std::endl;
        return 1;
    }
    std::cerr << "Found screenit functions" << std::endl;

    // Initialize LLVM
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    std::cerr << "Initialized native target" << std::endl;

    // Register the screenit functions with the JIT
    llvm::sys::DynamicLibrary::AddSymbol("screenit", (void*)screenit);
    llvm::sys::DynamicLibrary::AddSymbol("screenit_int", (void*)screenit_int);
    llvm::sys::DynamicLibrary::AddSymbol("screenit_double", (void*)screenit_double);
    std::cerr << "Registered screenit functions with JIT" << std::endl;

    // Read source file
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << argv[1] << std::endl;
        return 1;
    }
    std::string source((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    std::cerr << "Read source file: " << argv[1] << std::endl;
    std::cerr << "Source content:\n" << source << "\n" << std::endl;

    // Lexical analysis
    Lexer lexer(source);
    std::vector<Token> tokens = lexer.scanTokens();
    std::cerr << "Lexical analysis complete. Tokens:" << std::endl;
    for (const auto& token : tokens) {
        std::cerr << "  " << token.toString() << std::endl;
    }
    std::cerr << std::endl;

    // Parsing
    Parser parser(tokens);
    std::vector<std::unique_ptr<Stmt>> statements = parser.parse();
    std::cerr << "Parsing complete. Statements:" << std::endl;
    for (const auto& stmt : statements) {
        std::cerr << "  " << stmt->toString() << std::endl;
    }
    std::cerr << std::endl;

    // IR generation
    IRGenerator generator;
    std::unique_ptr<llvm::Module> module = generator.generate(statements);
    std::cerr << "IR generation complete" << std::endl;

    // Create execution engine
    std::string err;
    llvm::EngineBuilder builder(std::move(module));
    builder.setEngineKind(llvm::EngineKind::JIT);
    builder.setVerifyModules(true);
    builder.setOptLevel(llvm::CodeGenOptLevel::None);
    llvm::ExecutionEngine* engine = builder.setErrorStr(&err).create();

    if (!engine) {
        std::cerr << "Failed to create execution engine: " << err << std::endl;
        return 1;
    }
    std::cerr << "Created execution engine" << std::endl;

    // Register screenit functions with the engine
    engine->addGlobalMapping("screenit", (uint64_t)screenit);
    engine->addGlobalMapping("screenit_int", (uint64_t)screenit_int);
    engine->addGlobalMapping("screenit_double", (uint64_t)screenit_double);

    // Run the main function
    llvm::Function* mainFunc = engine->FindFunctionNamed("main");
    if (!mainFunc) {
        std::cerr << "Main function not found!" << std::endl;
        return 1;
    }
    std::cerr << "Found main function" << std::endl;

    std::cerr << "Running program..." << std::endl;
    llvm::GenericValue result = engine->runFunction(mainFunc, {});
    std::cerr << "Program execution complete" << std::endl;

    // Cleanup
    dlclose(handle);
    return 0;
}
