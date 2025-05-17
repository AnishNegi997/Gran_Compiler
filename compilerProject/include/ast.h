#pragma once

#include <string>
#include <vector>
#include <memory>
#include "lexer.h"

// Forward declarations
class Expr;
class Stmt;

// Expression visitor interface
class ExprVisitor {
public:
    virtual ~ExprVisitor() = default;
    virtual void visitBinaryExpr(class BinaryExpr* expr) = 0;
    virtual void visitUnaryExpr(class UnaryExpr* expr) = 0;
    virtual void visitLiteralExpr(class LiteralExpr* expr) = 0;
    virtual void visitVariableExpr(class VariableExpr* expr) = 0;
    virtual void visitAssignExpr(class AssignExpr* expr) = 0;
    virtual void visitCallExpr(class CallExpr* expr) = 0;
    virtual void visitGroupingExpr(class GroupingExpr* expr) = 0;
};

// Base expression class
class Expr {
public:
    virtual ~Expr() = default;
    virtual void accept(ExprVisitor* visitor) = 0;
    virtual std::string toString() const = 0;
};

// Binary expression (e.g., 1 + 2)
class BinaryExpr : public Expr {
public:
    std::unique_ptr<Expr> left;
    Token op;
    std::unique_ptr<Expr> right;

    BinaryExpr(std::unique_ptr<Expr> left, Token op, std::unique_ptr<Expr> right)
        : left(std::move(left)), op(op), right(std::move(right)) {}

    void accept(ExprVisitor* visitor) override {
        visitor->visitBinaryExpr(this);
    }

    std::string toString() const override {
        return "BinaryExpr(" + left->toString() + ", " + op.value + ", " + right->toString() + ")";
    }
};

// Unary expression (e.g., -5)
class UnaryExpr : public Expr {
public:
    Token op;
    std::unique_ptr<Expr> right;

    UnaryExpr(Token op, std::unique_ptr<Expr> right)
        : op(op), right(std::move(right)) {}

    void accept(ExprVisitor* visitor) override {
        visitor->visitUnaryExpr(this);
    }

    std::string toString() const override {
        return "UnaryExpr(" + op.value + ", " + right->toString() + ")";
    }
};

// Literal expression (e.g., 42, "hello")
class LiteralExpr : public Expr {
public:
    Token value;

    explicit LiteralExpr(Token value) : value(value) {}

    void accept(ExprVisitor* visitor) override {
        visitor->visitLiteralExpr(this);
    }

    std::string toString() const override {
        return "LiteralExpr(" + value.value + ")";
    }
};

// Variable expression (e.g., x)
class VariableExpr : public Expr {
public:
    Token name;

    explicit VariableExpr(Token name) : name(name) {}

    void accept(ExprVisitor* visitor) override {
        visitor->visitVariableExpr(this);
    }

    std::string toString() const override {
        return "VariableExpr(" + name.value + ")";
    }
};

// Assignment expression (e.g., x = 5)
class AssignExpr : public Expr {
public:
    Token name;
    std::unique_ptr<Expr> value;

    AssignExpr(Token name, std::unique_ptr<Expr> value)
        : name(name), value(std::move(value)) {}

    void accept(ExprVisitor* visitor) override {
        visitor->visitAssignExpr(this);
    }

    std::string toString() const override {
        return "AssignExpr(" + name.value + ", " + value->toString() + ")";
    }
};

// Function call expression (e.g., foo(x, y))
class CallExpr : public Expr {
public:
    Token callee;
    std::vector<std::unique_ptr<Expr>> arguments;

    CallExpr(Token callee, std::vector<std::unique_ptr<Expr>> arguments)
        : callee(callee), arguments(std::move(arguments)) {}

    void accept(ExprVisitor* visitor) override {
        visitor->visitCallExpr(this);
    }

    std::string toString() const override {
        std::string result = "CallExpr(" + callee.value + ", [";
        for (const auto& arg : arguments) {
            result += arg->toString() + ", ";
        }
        result += "])";
        return result;
    }
};

// Grouping expression (e.g., (1 + 2))
class GroupingExpr : public Expr {
public:
    std::unique_ptr<Expr> expression;

    explicit GroupingExpr(std::unique_ptr<Expr> expression)
        : expression(std::move(expression)) {}

    void accept(ExprVisitor* visitor) override {
        visitor->visitGroupingExpr(this);
    }

    std::string toString() const override {
        return "GroupingExpr(" + expression->toString() + ")";
    }
};

// Statement visitor interface
class StmtVisitor {
public:
    virtual ~StmtVisitor() = default;
    virtual void visitExpressionStmt(class ExprStmt* stmt) = 0;
    virtual void visitPrintStmt(class PrintStmt* stmt) = 0;
    virtual void visitVarStmt(class VarStmt* stmt) = 0;
    virtual void visitBlockStmt(class BlockStmt* stmt) = 0;
    virtual void visitIfStmt(class IfStmt* stmt) = 0;
    virtual void visitWhileStmt(class WhileStmt* stmt) = 0;
    virtual void visitForStmt(class ForStmt* stmt) = 0;
    virtual void visitFunctionStmt(class FunctionStmt* stmt) = 0;
    virtual void visitReturnStmt(class ReturnStmt* stmt) = 0;
};

// Base statement class
class Stmt {
public:
    virtual ~Stmt() = default;
    virtual void accept(StmtVisitor* visitor) = 0;
    virtual std::string toString() const = 0;
};

// Expression statement (e.g., x + 5;)
class ExprStmt : public Stmt {
public:
    std::unique_ptr<Expr> expression;

    explicit ExprStmt(std::unique_ptr<Expr> expression)
        : expression(std::move(expression)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitExpressionStmt(this);
    }

    std::string toString() const override {
        return "ExprStmt(" + expression->toString() + ")";
    }
};

// Print statement (e.g., print x;)
class PrintStmt : public Stmt {
public:
    std::unique_ptr<Expr> expression;

    explicit PrintStmt(std::unique_ptr<Expr> expression)
        : expression(std::move(expression)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitPrintStmt(this);
    }

    std::string toString() const override {
        return "PrintStmt(" + expression->toString() + ")";
    }
};

// Variable declaration statement (e.g., var x = 5;)
class VarStmt : public Stmt {
public:
    Token name;
    std::unique_ptr<Expr> initializer;

    VarStmt(Token name, std::unique_ptr<Expr> initializer)
        : name(name), initializer(std::move(initializer)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitVarStmt(this);
    }

    std::string toString() const override {
        return "VarStmt(" + name.value + ", " + (initializer ? initializer->toString() : "null") + ")";
    }
};

// Block statement (e.g., { x = 5; y = 6; })
class BlockStmt : public Stmt {
public:
    std::vector<std::unique_ptr<Stmt>> statements;

    explicit BlockStmt(std::vector<std::unique_ptr<Stmt>> statements)
        : statements(std::move(statements)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitBlockStmt(this);
    }

    std::string toString() const override {
        std::string result = "BlockStmt([";
        for (const auto& stmt : statements) {
            result += stmt->toString() + ", ";
        }
        result += "])";
        return result;
    }
};

// If statement (e.g., if (x > 5) { ... } else { ... })
class IfStmt : public Stmt {
public:
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> thenBranch;
    std::unique_ptr<Stmt> elseBranch;

    IfStmt(std::unique_ptr<Expr> condition,
           std::unique_ptr<Stmt> thenBranch,
           std::unique_ptr<Stmt> elseBranch)
        : condition(std::move(condition)),
          thenBranch(std::move(thenBranch)),
          elseBranch(std::move(elseBranch)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitIfStmt(this);
    }

    std::string toString() const override {
        return "IfStmt(" + condition->toString() + ", " + thenBranch->toString() + ", " + elseBranch->toString() + ")";
    }
};

// While statement (e.g., while (x > 0) { ... })
class WhileStmt : public Stmt {
public:
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Stmt> body;

    WhileStmt(std::unique_ptr<Expr> condition, std::unique_ptr<Stmt> body)
        : condition(std::move(condition)), body(std::move(body)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitWhileStmt(this);
    }

    std::string toString() const override {
        return "WhileStmt(" + condition->toString() + ", " + body->toString() + ")";
    }
};

// For statement (e.g., for (var i = 0; i < 10; i = i + 1) { ... })
class ForStmt : public Stmt {
public:
    std::unique_ptr<Stmt> initializer;
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Expr> increment;
    std::unique_ptr<Stmt> body;

    ForStmt(std::unique_ptr<Stmt> initializer,
            std::unique_ptr<Expr> condition,
            std::unique_ptr<Expr> increment,
            std::unique_ptr<Stmt> body)
        : initializer(std::move(initializer)),
          condition(std::move(condition)),
          increment(std::move(increment)),
          body(std::move(body)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitForStmt(this);
    }

    std::string toString() const override {
        return "ForStmt(" + initializer->toString() + ", " + condition->toString() + ", " + increment->toString() + ", " + body->toString() + ")";
    }
};

// Function declaration statement (e.g., fun foo(x, y) { ... })
class FunctionStmt : public Stmt {
public:
    Token name;
    std::vector<Token> params;
    std::vector<std::unique_ptr<Stmt>> body;

    FunctionStmt(Token name,
                 std::vector<Token> params,
                 std::vector<std::unique_ptr<Stmt>> body)
        : name(name), params(std::move(params)), body(std::move(body)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitFunctionStmt(this);
    }

    std::string toString() const override {
        std::string result = "FunctionStmt(" + name.value + ", [";
        for (const auto& param : params) {
            result += param.value + ", ";
        }
        result += "], [";
        for (const auto& stmt : body) {
            result += stmt->toString() + ", ";
        }
        result += "])";
        return result;
    }
};

// Return statement (e.g., return x;)
class ReturnStmt : public Stmt {
public:
    Token keyword;
    std::unique_ptr<Expr> value;

    ReturnStmt(Token keyword, std::unique_ptr<Expr> value)
        : keyword(keyword), value(std::move(value)) {}

    void accept(StmtVisitor* visitor) override {
        visitor->visitReturnStmt(this);
    }

    std::string toString() const override {
        return "ReturnStmt(" + keyword.value + ", " + (value ? value->toString() : "null") + ")";
    }
}; 