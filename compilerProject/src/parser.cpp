#include "../include/parser.h"
#include "../include/ast.h"
#include <stdexcept>
#include <iostream>

Parser::Parser(const std::vector<Token>& tokens) : tokens(tokens) {}

Token Parser::peek() const {
    return tokens[current];
}

Token Parser::advance() {
    if (!isAtEnd()) current++;
    return tokens[current - 1];
}

bool Parser::check(TokenType type) const {
    if (isAtEnd()) return false;
    return peek().type == type;
}

bool Parser::match(TokenType type) {
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

bool Parser::isAtEnd() const {
    return peek().type == TokenType::END_OF_FILE;
}

Token Parser::consume(TokenType type, const std::string& message) {
    if (check(type)) return advance();
    throw std::runtime_error(message);
}

std::vector<std::unique_ptr<Stmt>> Parser::parse() {
    std::vector<std::unique_ptr<Stmt>> statements;
    while (!isAtEnd()) {
        if (peek().type == TokenType::END_OF_FILE) {
            break;
        }
        if (peek().type == TokenType::UNKNOWN || peek().value.empty()) {
            advance();
            continue;
        }
        try {
            statements.push_back(declaration());
        } catch (const std::runtime_error& e) {
            if (isAtEnd()) break;
            throw;
        }
    }
    return statements;
}

std::unique_ptr<Stmt> Parser::declaration() {
    if (match(TokenType::KEYWORD)) {
        Token keyword = tokens[current - 1];
        if (keyword.value == "func") {
            return functionDeclaration();
        }
        if (keyword.value == "var") {
            return varDeclaration();
        }
        current--;
    }
    return statement();
}

std::unique_ptr<Stmt> Parser::functionDeclaration() {
    Token name = consume(TokenType::IDENTIFIER, "Expected function name.");

    if (!match(TokenType::LEFT_PAREN)) {
        throw std::runtime_error("Expected '(' after function name");
    }

    std::vector<Token> parameters;
    if (!check(TokenType::RIGHT_PAREN)) {
        do {
            if (parameters.size() >= 255) {
                throw std::runtime_error("Cannot have more than 255 parameters.");
            }

            parameters.push_back(consume(TokenType::IDENTIFIER, "Expected parameter name."));
        } while (match(TokenType::COMMA));
    }

    if (!match(TokenType::RIGHT_PAREN)) {
        throw std::runtime_error("Expected ')' after parameters.");
    }

    if (!match(TokenType::LEFT_BRACE)) {
        throw std::runtime_error("Expected '{' before function body.");
    }

    std::vector<std::unique_ptr<Stmt>> body;
    while (!check(TokenType::RIGHT_BRACE) && !isAtEnd()) {
        body.push_back(declaration());
    }

    if (!match(TokenType::RIGHT_BRACE)) {
        throw std::runtime_error("Expected '}' after function body.");
    }

    return std::make_unique<FunctionStmt>(name, parameters, std::move(body));
}

std::unique_ptr<Stmt> Parser::varDeclaration() {
    Token name = consume(TokenType::IDENTIFIER, "Expected variable name.");

    std::unique_ptr<Expr> initializer = nullptr;
    if (match(TokenType::OPERATOR) && previous().value == "=") {
        initializer = expression();
    }

    if (!match(TokenType::SEMICOLON)) {
        throw std::runtime_error("Expected ';' after variable declaration.");
    }

    return std::make_unique<VarStmt>(name, std::move(initializer));
}

std::unique_ptr<Stmt> Parser::statement() {
    if (match(TokenType::LEFT_BRACE)) return block();
    if (match(TokenType::KEYWORD)) {
        Token keyword = previous();
        if (keyword.value == "if") return ifStatement();
        if (keyword.value == "while") return whileStatement();
        if (keyword.value == "for") return forStatement();
        if (keyword.value == "screenit") return screenitStatement();
        if (keyword.value == "return") return returnStatement();
        if (keyword.value == "break") {
            if (!match(TokenType::SEMICOLON)) {
                throw std::runtime_error("Expected ';' after break.");
            }
            // Just skip break for now (no-op)
            return nullptr;
        }
        // If we get here, we found a keyword but it wasn't handled
        throw std::runtime_error("Unexpected keyword: " + keyword.value);
    }
    return expressionStatement();
}

std::unique_ptr<Stmt> Parser::screenitStatement() {
    // Skip the screenit keyword since we already matched it
    std::unique_ptr<Expr> value = expression();
    if (!match(TokenType::SEMICOLON)) {
        throw std::runtime_error("Expected ';' after screenit value.");
    }
    return std::make_unique<PrintStmt>(std::move(value));
}

std::unique_ptr<Stmt> Parser::returnStatement() {
    Token keyword = previous();
    std::unique_ptr<Expr> value = nullptr;
    if (!check(TokenType::SEMICOLON)) {
        value = expression();
    }

    if (!match(TokenType::SEMICOLON)) {
        throw std::runtime_error("Expected ';' after return value.");
    }

    return std::make_unique<ReturnStmt>(keyword, std::move(value));
}

std::unique_ptr<Stmt> Parser::ifStatement() {
    if (!match(TokenType::LEFT_PAREN)) {
        throw std::runtime_error("Expected '(' after 'if'.");
    }
    std::unique_ptr<Expr> condition = expression();
    if (!match(TokenType::RIGHT_PAREN)) {
        throw std::runtime_error("Expected ')' after if condition.");
    }

    std::unique_ptr<Stmt> thenBranch = statement();
    std::unique_ptr<Stmt> elseBranch = nullptr;
    if (match(TokenType::KEYWORD) && previous().value == "else") {
        elseBranch = statement();
    }

    return std::make_unique<IfStmt>(std::move(condition), std::move(thenBranch), std::move(elseBranch));
}

std::unique_ptr<Stmt> Parser::whileStatement() {
    if (!match(TokenType::LEFT_PAREN)) {
        throw std::runtime_error("Expected '(' after 'while'.");
    }
    std::unique_ptr<Expr> condition = expression();
    if (!match(TokenType::RIGHT_PAREN)) {
        throw std::runtime_error("Expected ')' after condition.");
    }
    std::unique_ptr<Stmt> body = statement();

    return std::make_unique<WhileStmt>(std::move(condition), std::move(body));
}

std::unique_ptr<Stmt> Parser::forStatement() {
    if (!match(TokenType::LEFT_PAREN)) {
        throw std::runtime_error("Expected '(' after 'for'.");
    }

    std::unique_ptr<Stmt> initializer;
    if (match(TokenType::SEMICOLON)) {
        initializer = nullptr;
    } else if (match(TokenType::KEYWORD) && previous().value == "var") {
        initializer = varDeclaration();
    } else {
        initializer = expressionStatement();
    }

    std::unique_ptr<Expr> condition = nullptr;
    if (!check(TokenType::SEMICOLON)) {
        condition = expression();
    }
    if (!match(TokenType::SEMICOLON)) {
        throw std::runtime_error("Expected ';' after loop condition.");
    }

    std::unique_ptr<Expr> increment = nullptr;
    if (!check(TokenType::RIGHT_PAREN)) {
        increment = expression();
    }
    if (!match(TokenType::RIGHT_PAREN)) {
        throw std::runtime_error("Expected ')' after for clauses.");
    }

    std::unique_ptr<Stmt> body = statement();

    if (increment != nullptr) {
        std::vector<std::unique_ptr<Stmt>> stmts;
        stmts.push_back(std::move(body));
        stmts.push_back(std::make_unique<ExprStmt>(std::move(increment)));
        body = std::make_unique<BlockStmt>(std::move(stmts));
    }

    if (condition == nullptr) {
        condition = std::make_unique<LiteralExpr>(Token(TokenType::BOOL_LITERAL, "true", 0, 0));
    }
    body = std::make_unique<WhileStmt>(std::move(condition), std::move(body));

    if (initializer != nullptr) {
        std::vector<std::unique_ptr<Stmt>> stmts;
        stmts.push_back(std::move(initializer));
        stmts.push_back(std::move(body));
        body = std::make_unique<BlockStmt>(std::move(stmts));
    }

    return body;
}

std::unique_ptr<Stmt> Parser::block() {
    std::vector<std::unique_ptr<Stmt>> statements;

    while (!check(TokenType::RIGHT_BRACE) && !isAtEnd()) {
        statements.push_back(declaration());
    }

    if (!match(TokenType::RIGHT_BRACE)) {
        throw std::runtime_error("Expected '}' after block.");
    }

    return std::make_unique<BlockStmt>(std::move(statements));
}

std::unique_ptr<Stmt> Parser::expressionStatement() {
    std::unique_ptr<Expr> expr = expression();
    if (!match(TokenType::SEMICOLON)) {
        throw std::runtime_error("Expected ';' after expression.");
    }
    return std::make_unique<ExprStmt>(std::move(expr));
}

std::unique_ptr<Expr> Parser::expression() {
    return assignment();
}

std::unique_ptr<Expr> Parser::assignment() {
    std::unique_ptr<Expr> expr = comparison();

    if (match(TokenType::OPERATOR) && tokens[current - 1].value == "=") {
        Token equals = tokens[current - 1];
        std::unique_ptr<Expr> value = assignment();

        if (auto varExpr = dynamic_cast<VariableExpr*>(expr.get())) {
            return std::make_unique<AssignExpr>(varExpr->name, std::move(value));
        }

        throw std::runtime_error("Invalid assignment target.");
    }

    return expr;
}

std::unique_ptr<Expr> Parser::comparison() {
    std::unique_ptr<Expr> expr = term();

    while (check(TokenType::COMPARE)) {
        Token op = advance();
        std::unique_ptr<Expr> right = term();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }

    return expr;
}

std::unique_ptr<Expr> Parser::term() {
    std::unique_ptr<Expr> expr = factor();

    while (match(TokenType::ARITHMETIC)) {
        Token op = tokens[current - 1];
        std::unique_ptr<Expr> right = factor();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }

    return expr;
}

std::unique_ptr<Expr> Parser::factor() {
    std::unique_ptr<Expr> expr = unary();

    while (match(TokenType::ARITHMETIC)) {
        Token op = tokens[current - 1];
        std::unique_ptr<Expr> right = unary();
        expr = std::make_unique<BinaryExpr>(std::move(expr), op, std::move(right));
    }

    return expr;
}

std::unique_ptr<Expr> Parser::unary() {
    if (match(TokenType::OPERATOR)) {
        Token op = tokens[current - 1];
        std::unique_ptr<Expr> right = unary();
        return std::make_unique<UnaryExpr>(op, std::move(right));
    }

    return call();
}

std::unique_ptr<Expr> Parser::call() {
    std::unique_ptr<Expr> expr = primary();

    while (true) {
        if (match(TokenType::LEFT_PAREN)) {
            expr = finishCall(std::move(expr));
        } else {
            break;
        }
    }

    return expr;
}

std::unique_ptr<Expr> Parser::primary() {
    if (match(TokenType::BOOL_LITERAL)) return std::make_unique<LiteralExpr>(previous());
    if (match(TokenType::INT_LITERAL)) return std::make_unique<LiteralExpr>(previous());
    if (match(TokenType::FLOAT_LITERAL)) return std::make_unique<LiteralExpr>(previous());
    if (match(TokenType::STRING_LITERAL)) return std::make_unique<LiteralExpr>(previous());

    if (match(TokenType::IDENTIFIER)) {
        return std::make_unique<VariableExpr>(previous());
    }

    if (match(TokenType::LEFT_PAREN)) {
        std::unique_ptr<Expr> expr = expression();
        if (!match(TokenType::RIGHT_PAREN)) {
            throw std::runtime_error("Expected ')' after expression.");
        }
        return std::make_unique<GroupingExpr>(std::move(expr));
    }

    throw std::runtime_error("Expected expression.");
}

std::unique_ptr<Expr> Parser::finishCall(std::unique_ptr<Expr> callee) {
    std::vector<std::unique_ptr<Expr>> arguments;
    if (!check(TokenType::RIGHT_PAREN)) {
        do {
            if (arguments.size() >= 255) {
                throw std::runtime_error("Cannot have more than 255 arguments.");
            }
            arguments.push_back(expression());
        } while (match(TokenType::COMMA));
    }

    if (!match(TokenType::RIGHT_PAREN)) {
        throw std::runtime_error("Expected ')' after arguments.");
    }

    auto* varExpr = dynamic_cast<VariableExpr*>(callee.get());
    if (!varExpr) {
        throw std::runtime_error("Expected function name for call expression.");
    }
    return std::make_unique<CallExpr>(varExpr->name, std::move(arguments));
} 