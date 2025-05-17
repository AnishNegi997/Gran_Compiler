#pragma once

#include "ast.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <memory>
#include <string>
#include <unordered_map>

class IRGenerator {
public:
    IRGenerator();
    ~IRGenerator();

    // Generate IR for the entire program
    std::unique_ptr<llvm::Module> generate(const std::vector<std::unique_ptr<Stmt>>& statements);

private:
    // LLVM context and builder
    llvm::LLVMContext context;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;

    // Symbol table for variables
    std::unordered_map<std::string, llvm::Value*> symbolTable;

    // Generate IR for statements
    void generateStmt(const Stmt* stmt);
    void generateExprStmt(const ExprStmt* stmt);
    void generatePrintStmt(const PrintStmt* stmt);
    void generateVarStmt(const VarStmt* stmt);
    void generateBlockStmt(const BlockStmt* stmt);
    void generateIfStmt(const IfStmt* stmt);
    void generateWhileStmt(const WhileStmt* stmt);
    void generateFunctionStmt(const FunctionStmt* stmt);
    void generateReturnStmt(const ReturnStmt* stmt);

    // Generate IR for expressions
    llvm::Value* generateExpr(const Expr* expr);
    llvm::Value* generateBinaryExpr(const BinaryExpr* expr);
    llvm::Value* generateUnaryExpr(const UnaryExpr* expr);
    llvm::Value* generateLiteralExpr(const LiteralExpr* expr);
    llvm::Value* generateVariableExpr(const VariableExpr* expr);
    llvm::Value* generateCallExpr(const CallExpr* expr);
    llvm::Value* generateGroupingExpr(const GroupingExpr* expr);
    llvm::Value* generateAssignExpr(const AssignExpr* expr);

    // Helper functions
    llvm::Type* getLLVMType(const Token& token);
    llvm::Value* getVariable(const std::string& name);
    void setVariable(const std::string& name, llvm::Value* value);
}; 