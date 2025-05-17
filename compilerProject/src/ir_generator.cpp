#include "../include/ir_generator.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <stdexcept>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/TargetSelect.h>

IRGenerator::IRGenerator() 
    : module(std::make_unique<llvm::Module>("main", context))
    , builder(context) {
}

IRGenerator::~IRGenerator() = default;

std::unique_ptr<llvm::Module> IRGenerator::generate(const std::vector<std::unique_ptr<Stmt>>& statements) {
    llvm::FunctionType* mainType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(context),
        false
    );
    llvm::Function* mainFunc = llvm::Function::Create(
        mainType,
        llvm::Function::ExternalLinkage,
        "main",
        module.get()
    );

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);

    for (const auto& stmt : statements) {
        generateStmt(stmt.get());
    }

    builder.CreateRet(llvm::ConstantInt::get(context, llvm::APInt(32, 0)));

    if (llvm::verifyModule(*module, &llvm::errs())) {
        throw std::runtime_error("Module verification failed");
    }

    return std::move(module);
}

void IRGenerator::generateStmt(const Stmt* stmt) {
    if (auto exprStmt = dynamic_cast<const ExprStmt*>(stmt)) {
        generateExprStmt(exprStmt);
    } else if (auto printStmt = dynamic_cast<const PrintStmt*>(stmt)) {
        generatePrintStmt(printStmt);
    } else if (auto varStmt = dynamic_cast<const VarStmt*>(stmt)) {
        generateVarStmt(varStmt);
    } else if (auto blockStmt = dynamic_cast<const BlockStmt*>(stmt)) {
        generateBlockStmt(blockStmt);
    } else if (auto ifStmt = dynamic_cast<const IfStmt*>(stmt)) {
        generateIfStmt(ifStmt);
    } else if (auto whileStmt = dynamic_cast<const WhileStmt*>(stmt)) {
        generateWhileStmt(whileStmt);
    } else if (auto funcStmt = dynamic_cast<const FunctionStmt*>(stmt)) {
        generateFunctionStmt(funcStmt);
    } else if (auto returnStmt = dynamic_cast<const ReturnStmt*>(stmt)) {
        generateReturnStmt(returnStmt);
    }
}

void IRGenerator::generateExprStmt(const ExprStmt* stmt) {
    generateExpr(stmt->expression.get());
}

void IRGenerator::generatePrintStmt(const PrintStmt* stmt) {
    llvm::Value* value = generateExpr(stmt->expression.get());
    if (!value) {
        throw std::runtime_error("Failed to generate expression for print statement");
    }

    // Create function types for different screenit variants
    llvm::Type* charPtrType = llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context));
    llvm::Type* intType = llvm::Type::getInt32Ty(context);
    llvm::Type* doubleType = llvm::Type::getDoubleTy(context);

    // Get or create screenit functions
    llvm::Function* screenitFunc = module->getFunction("screenit");
    if (!screenitFunc) {
        std::vector<llvm::Type*> args = {charPtrType};
        llvm::FunctionType* funcType = llvm::FunctionType::get(
            llvm::Type::getVoidTy(context),
            args,
            false
        );
        screenitFunc = llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            "screenit",
            module.get()
        );
    }

    llvm::Function* screenitIntFunc = module->getFunction("screenit_int");
    if (!screenitIntFunc) {
        std::vector<llvm::Type*> args = {intType};
        llvm::FunctionType* funcType = llvm::FunctionType::get(
            llvm::Type::getVoidTy(context),
            args,
            false
        );
        screenitIntFunc = llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            "screenit_int",
            module.get()
        );
    }

    llvm::Function* screenitDoubleFunc = module->getFunction("screenit_double");
    if (!screenitDoubleFunc) {
        std::vector<llvm::Type*> args = {doubleType};
        llvm::FunctionType* funcType = llvm::FunctionType::get(
            llvm::Type::getVoidTy(context),
            args,
            false
        );
        screenitDoubleFunc = llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            "screenit_double",
            module.get()
        );
    }

    // Prepare arguments and call the appropriate function
    std::vector<llvm::Value*> args;
    if (value->getType()->isPointerTy()) {
        // String value - use screenit
        args.push_back(value);
        builder.CreateCall(screenitFunc, args);
    } else if (value->getType()->isIntegerTy()) {
        // Integer value - use screenit_int
        args.push_back(value);
        builder.CreateCall(screenitIntFunc, args);
    } else if (value->getType()->isDoubleTy()) {
        // Float value - use screenit_double
        args.push_back(value);
        builder.CreateCall(screenitDoubleFunc, args);
    } else {
        throw std::runtime_error("Unsupported type in print statement");
    }
}

void IRGenerator::generateVarStmt(const VarStmt* stmt) {
    llvm::Value* initValue = nullptr;
    if (stmt->initializer) {
        initValue = generateExpr(stmt->initializer.get());
    } else {
        initValue = llvm::ConstantInt::get(context, llvm::APInt(32, 0));
    }

    llvm::AllocaInst* alloca = builder.CreateAlloca(initValue->getType(), nullptr, stmt->name.value);
    builder.CreateStore(initValue, alloca);
    setVariable(stmt->name.value, alloca);
}

void IRGenerator::generateBlockStmt(const BlockStmt* stmt) {
    std::unordered_map<std::string, llvm::Value*> oldSymbolTable = symbolTable;
    for (const auto& stmt : stmt->statements) {
        generateStmt(stmt.get());
    }
    symbolTable = oldSymbolTable;
}

void IRGenerator::generateIfStmt(const IfStmt* stmt) {
    llvm::Value* cond = generateExpr(stmt->condition.get());
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "else", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "ifcont", func);

    builder.CreateCondBr(cond, thenBB, elseBB);

    builder.SetInsertPoint(thenBB);
    generateStmt(stmt->thenBranch.get());
    builder.CreateBr(mergeBB);

    builder.SetInsertPoint(elseBB);
    if (stmt->elseBranch) {
        generateStmt(stmt->elseBranch.get());
    }
    builder.CreateBr(mergeBB);

    builder.SetInsertPoint(mergeBB);
}

void IRGenerator::generateWhileStmt(const WhileStmt* stmt) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context, "whilecond", func);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context, "whilebody", func);
    llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(context, "whileafter", func);

    builder.CreateBr(condBB);
    builder.SetInsertPoint(condBB);

    llvm::Value* cond = generateExpr(stmt->condition.get());
    builder.CreateCondBr(cond, bodyBB, afterBB);

    builder.SetInsertPoint(bodyBB);
    generateStmt(stmt->body.get());
    builder.CreateBr(condBB);

    builder.SetInsertPoint(afterBB);
}

void IRGenerator::generateFunctionStmt(const FunctionStmt* stmt) {
    // TODO: Implement function generation
}

void IRGenerator::generateReturnStmt(const ReturnStmt* stmt) {
    if (stmt->value) {
        llvm::Value* retVal = generateExpr(stmt->value.get());
        builder.CreateRet(retVal);
    } else {
        builder.CreateRetVoid();
    }
}

llvm::Value* IRGenerator::generateExpr(const Expr* expr) {
    if (auto binary = dynamic_cast<const BinaryExpr*>(expr)) {
        return generateBinaryExpr(binary);
    } else if (auto unary = dynamic_cast<const UnaryExpr*>(expr)) {
        return generateUnaryExpr(unary);
    } else if (auto literal = dynamic_cast<const LiteralExpr*>(expr)) {
        return generateLiteralExpr(literal);
    } else if (auto variable = dynamic_cast<const VariableExpr*>(expr)) {
        return generateVariableExpr(variable);
    } else if (auto call = dynamic_cast<const CallExpr*>(expr)) {
        return generateCallExpr(call);
    } else if (auto grouping = dynamic_cast<const GroupingExpr*>(expr)) {
        return generateGroupingExpr(grouping);
    } else if (auto assign = dynamic_cast<const AssignExpr*>(expr)) {
        return generateAssignExpr(assign);
    }
    return nullptr;
}

llvm::Value* IRGenerator::generateBinaryExpr(const BinaryExpr* expr) {
    llvm::Value* left = generateExpr(expr->left.get());
    llvm::Value* right = generateExpr(expr->right.get());

    if (!left || !right) {
        throw std::runtime_error("Failed to generate binary expression operands");
    }

    // Load values if they are pointers
    if (left->getType()->isPointerTy()) {
        left = builder.CreateLoad(builder.getInt32Ty(), left);
    }
    if (right->getType()->isPointerTy()) {
        right = builder.CreateLoad(builder.getInt32Ty(), right);
    }

    if (left->getType() != right->getType()) {
        throw std::runtime_error("Type mismatch in binary expression");
    }

    if (expr->op.type == TokenType::ARITHMETIC) {
        if (expr->op.value == "+") return builder.CreateAdd(left, right, "addtmp");
        if (expr->op.value == "-") return builder.CreateSub(left, right, "subtmp");
        if (expr->op.value == "*") return builder.CreateMul(left, right, "multmp");
        if (expr->op.value == "/") return builder.CreateSDiv(left, right, "divtmp");
    } else if (expr->op.type == TokenType::COMPARE) {
        if (expr->op.value == "<") return builder.CreateICmpSLT(left, right, "cmptmp");
        if (expr->op.value == ">") return builder.CreateICmpSGT(left, right, "cmptmp");
        if (expr->op.value == "<=") return builder.CreateICmpSLE(left, right, "cmptmp");
        if (expr->op.value == ">=") return builder.CreateICmpSGE(left, right, "cmptmp");
        if (expr->op.value == "==") return builder.CreateICmpEQ(left, right, "cmptmp");
        if (expr->op.value == "!=") return builder.CreateICmpNE(left, right, "cmptmp");
    }

    throw std::runtime_error("Unsupported binary operator: " + expr->op.value);
}

llvm::Value* IRGenerator::generateUnaryExpr(const UnaryExpr* expr) {
    llvm::Value* operand = generateExpr(expr->right.get());
    if (!operand) {
        throw std::runtime_error("Failed to generate unary expression operand");
    }

    if (expr->op.value == "-") return builder.CreateNeg(operand, "negtmp");
    if (expr->op.value == "!") return builder.CreateNot(operand, "nottmp");

    throw std::runtime_error("Unsupported unary operator: " + expr->op.value);
}

llvm::Value* IRGenerator::generateLiteralExpr(const LiteralExpr* expr) {
    if (expr->value.type == TokenType::INT_LITERAL) {
        return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->value.value)));
    } else if (expr->value.type == TokenType::FLOAT_LITERAL) {
        return llvm::ConstantFP::get(context, llvm::APFloat(std::stof(expr->value.value)));
    } else if (expr->value.type == TokenType::BOOL_LITERAL) {
        return llvm::ConstantInt::get(context, llvm::APInt(1, expr->value.value == "true"));
    } else if (expr->value.type == TokenType::STRING_LITERAL) {
        // Remove quotes from string literal
        std::string str = expr->value.value;
        if (str.size() >= 2 && str.front() == '"' && str.back() == '"') {
            str = str.substr(1, str.size() - 2);
        }
        return builder.CreateGlobalStringPtr(str);
    }
    return nullptr;
}

llvm::Value* IRGenerator::generateVariableExpr(const VariableExpr* expr) {
    llvm::Value* alloca = getVariable(expr->name.value);
    return builder.CreateLoad(builder.getInt32Ty(), alloca);
}

llvm::Value* IRGenerator::generateCallExpr(const CallExpr* expr) {
    // TODO: Implement function calls
    return nullptr;
}

llvm::Value* IRGenerator::generateGroupingExpr(const GroupingExpr* expr) {
    return generateExpr(expr->expression.get());
}

llvm::Value* IRGenerator::generateAssignExpr(const AssignExpr* expr) {
    llvm::Value* value = generateExpr(expr->value.get());
    llvm::Value* variable = getVariable(expr->name.value);
    builder.CreateStore(value, variable);
    return value;
}

llvm::Value* IRGenerator::getVariable(const std::string& name) {
    auto it = symbolTable.find(name);
    if (it == symbolTable.end()) {
        throw std::runtime_error("Undefined variable: " + name);
    }
    return it->second;  // Return the alloca instruction directly
}

void IRGenerator::setVariable(const std::string& name, llvm::Value* value) {
    if (!value) {
        throw std::runtime_error("Cannot set null value for variable: " + name);
    }
    symbolTable[name] = value;
}
