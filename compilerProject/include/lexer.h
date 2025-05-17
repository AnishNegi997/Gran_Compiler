#pragma once
#include <string>
#include <vector>
#include <unordered_map>

enum class TokenType {
    // Keywords
    KEYWORD,
    
    // Literals
    INT_LITERAL,
    FLOAT_LITERAL,
    STRING_LITERAL,
    BOOL_LITERAL,
    
    // Identifiers
    IDENTIFIER,
    
    // Operators
    OPERATOR,
    ARITHMETIC,
    COMPARE,
    
    // Separators
    LEFT_PAREN,    // (
    RIGHT_PAREN,   // )
    LEFT_BRACE,    // {
    RIGHT_BRACE,   // }
    SEMICOLON,     // ;
    COMMA,         // ,
    
    // Special
    UNKNOWN,
    END_OF_FILE
};

struct Token {
    TokenType type;
    std::string value;
    int line;
    int column;

    Token(TokenType type, const std::string& value, int line, int column)
        : type(type), value(value), line(line), column(column) {}

    std::string toString() const {
        return "Token(" + std::to_string(static_cast<int>(type)) + ", \"" + value + "\", " + std::to_string(line) + ", " + std::to_string(column) + ")";
    }
};

class Lexer {
public:
    explicit Lexer(const std::string& source);
    std::vector<Token> scanTokens();
    Token scanToken();

private:
    std::string source;
    std::vector<Token> tokens;
    size_t current;
    size_t start;
    size_t line;
    size_t column;

    bool isAtEnd() const;
    void scanNextToken();
    char advance();
    void addToken(TokenType type, const std::string& value);
    bool match(char expected);
    char peek() const;
    char peekNext() const;
    void string();
    void number();
    void identifier();
    bool isDigit(char c) const;
    bool isAlpha(char c) const;
    bool isAlphaNumeric(char c) const;
};
