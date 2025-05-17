#include "../include/transpiler.h"
#include <sstream>
#include <stdexcept>

Transpiler::Transpiler(const std::vector<Token>& tokens) : tokens(tokens), current(0) {}

std::string Transpiler::transpile() {
    std::stringstream output;
    output << "#include <iostream>\n";
    output << "#include <string>\n\n";
    
    while (!isAtEnd()) {
        try {
            output << transpileStatement();
        } catch (const std::runtime_error& e) {
            throw std::runtime_error("Error at token " + std::to_string(current) + ": " + e.what());
        }
    }
    
    return output.str();
}

bool Transpiler::isAtEnd() const {
    return current >= tokens.size();
}

Token Transpiler::advance() {
    if (!isAtEnd()) current++;
    return previous();
}

bool Transpiler::check(TokenType type) const {
    if (isAtEnd()) return false;
    return peek().type == type;
}

Token Transpiler::peek() const {
    return tokens[current];
}

Token Transpiler::previous() const {
    return tokens[current - 1];
}

bool Transpiler::match(TokenType type) {
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

std::string Transpiler::transpileStatement() {
    if (match(TokenType::KEYWORD)) {
        if (previous().value == "func") {
            return transpileFunction();
        } else if (previous().value == "print") {
            return transpilePrint();
        } else if (previous().value == "return") {
            return transpileReturn();
        } else if (previous().value == "var") {
            return transpileVariable();
        }
    }
    
    throw std::runtime_error("Unexpected statement");
}

std::string Transpiler::transpileFunction() {
    std::stringstream output;
    
    // Function name
    if (!match(TokenType::IDENTIFIER)) {
        throw std::runtime_error("Expected function name");
    }
    std::string funcName = previous().value;
    
    // Parameters
    if (!match(TokenType::LEFT_PAREN)) {
        throw std::runtime_error("Expected '(' after function name");
    }
    
    output << "void " << funcName << "() {\n";
    
    // Function body
    if (!match(TokenType::LEFT_BRACE)) {
        throw std::runtime_error("Expected '{' after function parameters");
    }
    
    while (!check(TokenType::RIGHT_BRACE)) {
        output << transpileStatement();
    }
    
    if (!match(TokenType::RIGHT_BRACE)) {
        throw std::runtime_error("Expected '}' after function body");
    }
    
    output << "}\n\n";
    return output.str();
}

std::string Transpiler::transpilePrint() {
    std::stringstream output;
    
    if (!match(TokenType::LEFT_PAREN)) {
        throw std::runtime_error("Expected '(' after print");
    }
    
    output << "std::cout << ";
    
    // Print argument
    if (match(TokenType::STRING_LITERAL)) {
        output << "\"" << previous().value << "\"";
    } else if (match(TokenType::IDENTIFIER)) {
        output << previous().value;
    } else {
        throw std::runtime_error("Expected string or identifier after print");
    }
    
    if (!match(TokenType::RIGHT_PAREN)) {
        throw std::runtime_error("Expected ')' after print argument");
    }
    
    output << " << std::endl;\n";
    return output.str();
}

std::string Transpiler::transpileReturn() {
    std::stringstream output;
    output << "return";
    
    if (!match(TokenType::SEMICOLON)) {
        throw std::runtime_error("Expected ';' after return");
    }
    
    output << ";\n";
    return output.str();
}

std::string Transpiler::transpileVariable() {
    std::stringstream output;
    
    if (!match(TokenType::IDENTIFIER)) {
        throw std::runtime_error("Expected variable name");
    }
    std::string varName = previous().value;
    
    if (!match(TokenType::OPERATOR) || previous().value != "=") {
        throw std::runtime_error("Expected '=' after variable name");
    }
    
    output << "auto " << varName << " = ";
    
    // Variable value
    if (match(TokenType::STRING_LITERAL)) {
        output << "\"" << previous().value << "\"";
    } else if (match(TokenType::FLOAT_LITERAL)) {
        output << previous().value;
    } else if (match(TokenType::IDENTIFIER)) {
        output << previous().value;
    } else {
        throw std::runtime_error("Expected value after '='");
    }
    
    if (!match(TokenType::SEMICOLON)) {
        throw std::runtime_error("Expected ';' after variable declaration");
    }
    
    output << ";\n";
    return output.str();
} 