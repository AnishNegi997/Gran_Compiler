#include "../include/lexer.h"
#include <cctype>
#include <stdexcept>
#include <unordered_map>
#include <algorithm>
#include <iostream>

static const std::unordered_map<std::string, TokenType> keywords = {
    {"func", TokenType::KEYWORD},
    {"if", TokenType::KEYWORD},
    {"else", TokenType::KEYWORD},
    {"while", TokenType::KEYWORD},
    {"for", TokenType::KEYWORD},
    {"screenit", TokenType::KEYWORD},
    {"return", TokenType::KEYWORD},
    {"var", TokenType::KEYWORD},
    {"true", TokenType::BOOL_LITERAL},
    {"false", TokenType::BOOL_LITERAL},
    {"break", TokenType::KEYWORD}
};

// Helper function to trim whitespace
static std::string trim(const std::string& str) {
    size_t first = str.find_first_not_of(" \t\n\r");
    if (first == std::string::npos) return "";
    size_t last = str.find_last_not_of(" \t\n\r");
    return str.substr(first, (last - first + 1));
}

Lexer::Lexer(const std::string& source) : source(source), current(0), start(0), line(1), column(1) {}

std::vector<Token> Lexer::scanTokens() {
    std::vector<Token> result;
    while (!isAtEnd()) {
        start = current;
        scanNextToken();
        if (!tokens.empty()) {
            result.push_back(tokens.back());
            tokens.pop_back();
        }
    }
    result.emplace_back(TokenType::END_OF_FILE, "", line, column);
    return result;
}

Token Lexer::scanToken() {
    if (isAtEnd()) {
        return Token(TokenType::END_OF_FILE, "", line, column);
    }
    start = current;
    scanNextToken();
    if (!tokens.empty()) {
        Token token = tokens.back();
        tokens.pop_back();
        return token;
    }
    return Token(TokenType::UNKNOWN, "", line, column);
}

bool Lexer::isAtEnd() const {
    return current >= source.length();
}

void Lexer::addToken(TokenType type, const std::string& value) {
    tokens.emplace_back(type, value, line, column);
}

void Lexer::scanNextToken() {
    char c = advance();
    switch (c) {
        case '(': addToken(TokenType::LEFT_PAREN, "("); break;
        case ')': addToken(TokenType::RIGHT_PAREN, ")"); break;
        case '{': addToken(TokenType::LEFT_BRACE, "{"); break;
        case '}': addToken(TokenType::RIGHT_BRACE, "}"); break;
        case ';': addToken(TokenType::SEMICOLON, ";"); break;
        case ',': addToken(TokenType::COMMA, ","); break;
        case '+': addToken(TokenType::ARITHMETIC, "+"); break;
        case '-': addToken(TokenType::ARITHMETIC, "-"); break;
        case '*': addToken(TokenType::ARITHMETIC, "*"); break;
        case '/': 
            if (match('/')) {
                // A comment goes until the end of the line.
                while (peek() != '\n' && !isAtEnd()) advance();
            } else {
                addToken(TokenType::ARITHMETIC, "/");
            }
            break;
        case '=': addToken(TokenType::OPERATOR, "="); break;
        case '!': addToken(TokenType::OPERATOR, "!"); break;
        case '<': addToken(TokenType::COMPARE, "<"); break;
        case '>': addToken(TokenType::COMPARE, ">"); break;
        case ' ': case '\r': case '\t': break;
        case '\n': line++; column = 1; break;
        case '"': string(); break;
        default:
            if (isDigit(c)) {
                number();
            } else if (isAlpha(c)) {
                identifier();
            } else {
                addToken(TokenType::UNKNOWN, std::string(1, c));
            }
            break;
    }
}

char Lexer::advance() {
    return source[current++];
}

bool Lexer::match(char expected) {
    if (isAtEnd() || source[current] != expected) return false;
    current++;
    return true;
}

char Lexer::peek() const {
    if (isAtEnd()) return '\0';
    return source[current];
}

char Lexer::peekNext() const {
    if (current + 1 >= source.length()) return '\0';
    return source[current + 1];
}

void Lexer::string() {
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') line++;
        advance();
    }
    if (isAtEnd()) {
        addToken(TokenType::UNKNOWN, "Unterminated string.");
        return;
    }
    advance(); // The closing ".
    std::string value = source.substr(start + 1, current - start - 2);
    addToken(TokenType::STRING_LITERAL, value);
}

void Lexer::number() {
    while (isDigit(peek())) advance();
    if (peek() == '.' && isDigit(peekNext())) {
        advance();
        while (isDigit(peek())) advance();
        addToken(TokenType::FLOAT_LITERAL, source.substr(start, current - start));
    } else {
        addToken(TokenType::INT_LITERAL, source.substr(start, current - start));
    }
}

void Lexer::identifier() {
    while (isAlphaNumeric(peek())) advance();
    std::string text = source.substr(start, current - start);
    auto it = keywords.find(text);
    if (it != keywords.end()) {
        addToken(it->second, text);
    } else {
        addToken(TokenType::IDENTIFIER, text);
    }
}

bool Lexer::isDigit(char c) const {
    return std::isdigit(c);
}

bool Lexer::isAlpha(char c) const {
    return std::isalpha(c) || c == '_';
}

bool Lexer::isAlphaNumeric(char c) const {
    return isAlpha(c) || isDigit(c);
}
