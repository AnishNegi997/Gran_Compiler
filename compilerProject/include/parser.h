#pragma once
#include <string>
#include <vector>
#include <memory>
#include <stdexcept>
#include "lexer.h"
#include "ast.h"

// Parser class
class Parser {
private:
    std::vector<Token> tokens;
    size_t current = 0;

    Token peek() const;
    Token advance();
    bool check(TokenType type) const;
    bool match(TokenType type);
    bool isAtEnd() const;
    Token consume(TokenType type, const std::string& message);
    Token previous() const { return tokens[current - 1]; }

    // Expression parsing methods
    std::unique_ptr<Expr> expression();
    std::unique_ptr<Expr> assignment();
    std::unique_ptr<Expr> comparison();
    std::unique_ptr<Expr> term();
    std::unique_ptr<Expr> factor();
    std::unique_ptr<Expr> unary();
    std::unique_ptr<Expr> call();
    std::unique_ptr<Expr> primary();
    std::unique_ptr<Expr> finishCall(std::unique_ptr<Expr> callee);

    // Statement parsing methods
    std::unique_ptr<Stmt> statement();
    std::unique_ptr<Stmt> screenitStatement();
    std::unique_ptr<Stmt> expressionStatement();
    std::unique_ptr<Stmt> ifStatement();
    std::unique_ptr<Stmt> whileStatement();
    std::unique_ptr<Stmt> forStatement();
    std::unique_ptr<Stmt> returnStatement();
    std::unique_ptr<Stmt> block();
    std::unique_ptr<Stmt> declaration();
    std::unique_ptr<Stmt> varDeclaration();
    std::unique_ptr<Stmt> functionDeclaration();

public:
    Parser(const std::vector<Token>& tokens);
    std::vector<std::unique_ptr<Stmt>> parse();
}; 