#pragma once

#include "lexer.h"
#include <string>
#include <vector>

class Transpiler {
public:
    explicit Transpiler(const std::vector<Token>& tokens);
    std::string transpile();

private:
    std::vector<Token> tokens;
    size_t current;

    bool isAtEnd() const;
    Token advance();
    bool check(TokenType type) const;
    Token peek() const;
    Token previous() const;
    bool match(TokenType type);

    std::string transpileStatement();
    std::string transpileFunction();
    std::string transpilePrint();
    std::string transpileReturn();
    std::string transpileVariable();
}; 