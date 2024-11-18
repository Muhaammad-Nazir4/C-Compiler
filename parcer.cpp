#include <iostream>
#include <vector>
#include <string>
#include <cctype>
#include <map>
#include <unordered_map>
#include <fstream>
#include <iomanip> // For setting the table width

using namespace std;
// ANSI escape codes for coloring
const string RESET = "\033[0m";
const string RED = "\033[31m";
const string GREEN = "\033[32m";
const string YELLOW = "\033[33m";
const string BLUE = "\033[34m";
const string CYAN = "\033[36m";
const string MAGENTA = "\033[35m";


enum TokenType {
    T_INT, T_FLOAT, T_DOUBLE, T_STRING, T_BOOL, T_CHAR,
    T_ID, T_NUM, T_IF, T_ELSE, T_RETURN, T_AGAR, T_WHILE,
    T_ASSIGN, T_PLUS, T_MINUS, T_MUL, T_DIV, 
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,  
    T_SEMICOLON, T_GT, T_LT, T_EQ, T_NEQ, T_AND, T_OR, T_EOF,
};

struct TAC {
    string op;       // Operator (e.g., +, -, *, /, =, etc.)
    string arg1;     // First operand
    string arg2;     // Second operand (can be empty for unary operators)
    string result;   // Result variable
};
vector<TAC> tacInstructions; // Store all TAC instructions
int tempCounter = 0;         // Temporary variable counter
string generateTempVar() {
    return "t" + to_string(tempCounter++);
}


struct Token {
    TokenType type;
    string value;
    int line;
};

struct SymbolInfo {
    string type; 
    string value;
    int line; 
};

unordered_map<string, SymbolInfo> symbolTable;

class Lexer {
private:
    string src;
    size_t pos;
    int line;

    // List of keywords and their corresponding token types
    unordered_map<string, TokenType> keywords = {
        {"int", T_INT}, {"float", T_FLOAT}, {"double", T_DOUBLE},
        {"string", T_STRING}, {"bool", T_BOOL}, {"char", T_CHAR},
        {"if", T_IF}, {"Agar", T_AGAR}, {"else", T_ELSE},
        {"return", T_RETURN}, {"while", T_WHILE}
    };

public:
    Lexer(const string &src) {
        this->src = src;  
        this->pos = 0;
        this->line = 1;  
    }

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < src.size()) {
            char current = src[pos];
            
            if (isspace(current)) {
                if (current == '\n') line++;  
                pos++;
                continue;
            }
            if (current == '/' && peek() == '/') {
                skipLine();
                continue;
            }
            if (isdigit(current)) {
                string number = consumeNumber();
                if (number.find('.') != string::npos) {
                    tokens.push_back(Token{T_FLOAT, number, line});
                } else {
                    tokens.push_back(Token{T_NUM, number, line});
                }
                continue;
            }
            if (isalpha(current)) {
                string word = consumeWord();
                TokenType type;

                // Check if the word is a keyword
                if (keywords.find(word) != keywords.end()) {
                    type = keywords[word];
                    tokens.push_back(Token{type, word, line});

                    // Add the keyword to the symbol table with its type
                    if (symbolTable.find(word) == symbolTable.end()) {
                        symbolTable[word] = {word, "", line};
                    }
                } else {
                    tokens.push_back(Token{T_ID, word, line});
                }
                continue;
            }
            if (current == '\'') {
                tokens.push_back(Token{T_CHAR, consumeCharLiteral(), line});
                continue;
            }
            if (current == '\"') {
                tokens.push_back(Token{T_STRING, consumeStringLiteral(), line});
                continue;
            }

            switch (current) {
                case '=': 
                    if (peek() == '=') { pos++; tokens.push_back(Token{T_EQ, "==", line}); }
                    else tokens.push_back(Token{T_ASSIGN, "=", line});
                    break;
                case '+': tokens.push_back(Token{T_PLUS, "+", line}); break;
                case '-': tokens.push_back(Token{T_MINUS, "-", line}); break;
                case '*': tokens.push_back(Token{T_MUL, "*", line}); break;
                case '/': tokens.push_back(Token{T_DIV, "/", line}); break;
                case '(': tokens.push_back(Token{T_LPAREN, "(", line}); break;
                case ')': tokens.push_back(Token{T_RPAREN, ")", line}); break;
                case '{': tokens.push_back(Token{T_LBRACE, "{", line}); break;
                case '}': tokens.push_back(Token{T_RBRACE, "}", line}); break;
                case ';': tokens.push_back(Token{T_SEMICOLON, ";", line}); break;
                case '>': tokens.push_back(Token{T_GT, ">", line}); break;
                case '<': tokens.push_back(Token{T_LT, "<", line}); break;
                case '!':
                    if (peek() == '=') { pos++; tokens.push_back(Token{T_NEQ, "!=", line}); }
                    break;
                case '&':
                    if (peek() == '&') { pos++; tokens.push_back(Token{T_AND, "&&", line}); }
                    break;
                case '|':
                    if (peek() == '|') { pos++; tokens.push_back(Token{T_OR, "||", line}); }
                    break;
                default: 
                    cout << "Unexpected character: " << current << " on line " << line << endl; 
                    exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", line});
        return tokens;
    }

    char peek() {
        return pos + 1 < src.size() ? src[pos + 1] : '\0';
    }

    void skipLine() {
        while (pos < src.size() && src[pos] != '\n') {
            pos++;
        }
    }
    
    string consumeNumber() {
        size_t start = pos;
        bool isFloat = false;

        while (pos < src.size() && isdigit(src[pos])) pos++;

        if (pos < src.size() && src[pos] == '.') {
            isFloat = true;
            pos++; 
            while (pos < src.size() && isdigit(src[pos])) pos++;
        }

        string number = src.substr(start, pos - start);
        return isFloat ? number : number;
    }

    string consumeCharLiteral() {
        pos++; 
        if (pos < src.size() && src[pos + 1] == '\'') {
            string charLiteral(1, src[pos]);
            pos += 2; 
            return charLiteral;
        }
        cout << "Error: Invalid character literal on line " << line << endl;
        exit(1);
    }

    string consumeStringLiteral() {
        pos++; 
        size_t start = pos;
        while (pos < src.size() && src[pos] != '\"') {
            pos++;
        }
        if (pos < src.size()) {
            string strLiteral = src.substr(start, pos - start);
            pos++; 
            return strLiteral;
        }
        cout << "Error: Unterminated string literal on line " << line << endl;
        exit(1);
    }

    string consumeWord() {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos])) pos++;
        return src.substr(start, pos - start);
    }
};

class Parser {
private:
    vector<Token> tokens;
    size_t pos;

public:
    Parser(const vector<Token> &tokens) {
        this->tokens = tokens;  
        this->pos = 0;          
    }

    void parseProgram() {
        while (tokens[pos].type != T_EOF) {
            parseStatement();
        }
        cout << "Parsing completed successfully! No Syntax Error" << endl;
    }

private:
    void parseStatement() {
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT || 
            tokens[pos].type == T_DOUBLE || tokens[pos].type == T_STRING || 
            tokens[pos].type == T_BOOL || tokens[pos].type == T_CHAR) {
            parseDeclaration();
        } else if (tokens[pos].type == T_ID) {
            parseAssignment();
        } else if (tokens[pos].type == T_IF || tokens[pos].type == T_AGAR) {
            parseIfStatement();
        } else if (tokens[pos].type == T_RETURN) {
            parseReturnStatement();
        } else if (tokens[pos].type == T_WHILE) {
            parseWhileStatement();
        } else if (tokens[pos].type == T_LBRACE) {  
            parseBlock();
        } else {
            cout << "Syntax error: unexpected token " << tokens[pos].value << " on line " << tokens[pos].line << endl;
            exit(1);
        }
    }

    void parseBlock() {
        expect(T_LBRACE);  
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF) {
            parseStatement();
        }
        expect(T_RBRACE);  
    }

    void parseDeclaration() {
        TokenType varType = tokens[pos].type;
        string typeString;
        switch (varType) {
            case T_INT: typeString = "int"; break;
            case T_FLOAT: typeString = "float"; break;
            case T_DOUBLE: typeString = "double"; break;
            case T_STRING: typeString = "string"; break;
            case T_BOOL: typeString = "bool"; break;
            case T_CHAR: typeString = "char"; break;
            default: typeString = "unknown"; break; 
        }
        
        pos++;  
        expect(T_ID);
        string varName = tokens[pos - 1].value;

        symbolTable[varName] = {typeString, "0", tokens[pos - 1].line}; 
        expect(T_SEMICOLON);
    }

    void parseAssignment() {
        string varName = tokens[pos].value;
        expect(T_ID);
        expect(T_ASSIGN);
        string value = parseExpression();
        expect(T_SEMICOLON);

        if (symbolTable.find(varName) != symbolTable.end()) {
            symbolTable[varName].value = value;
        } else {
            cout << "Error: Undeclared variable " << varName << " on line " << tokens[pos].line << endl;
            exit(1);
        }
    }

    void parseIfStatement() {
        if (tokens[pos].type == T_IF || tokens[pos].type == T_AGAR) pos++;
        expect(T_LPAREN);
        parseExpression();
        expect(T_RPAREN);
        parseStatement();  
        if (tokens[pos].type == T_ELSE) {
            expect(T_ELSE);
            parseStatement();  
        }
    }

    void parseWhileStatement() {
        expect(T_WHILE);
        expect(T_LPAREN);
        parseExpression();
        expect(T_RPAREN);
        parseStatement();
    }

    void parseReturnStatement() {
        expect(T_RETURN);
        string value = parseExpression();
        expect(T_SEMICOLON);
    }

string parseExpression() {
    string leftValue = parseTerm();

    while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS ||
           tokens[pos].type == T_GT || tokens[pos].type == T_LT ||
           tokens[pos].type == T_EQ || tokens[pos].type == T_NEQ ||
           tokens[pos].type == T_AND || tokens[pos].type == T_OR) {
        TokenType op = tokens[pos].type;
        string opStr;

        switch (op) {
            case T_PLUS: opStr = "+"; break;
            case T_MINUS: opStr = "-"; break;
            case T_GT: opStr = ">"; break;
            case T_LT: opStr = "<"; break;
            case T_EQ: opStr = "=="; break;
            case T_NEQ: opStr = "!="; break;
            case T_AND: opStr = "&&"; break;
            case T_OR: opStr = "||"; break;
            default: opStr = "";
        }

        pos++;
        string rightValue = parseTerm();
        string tempVar = generateTempVar();

        // Add a TAC instruction
        tacInstructions.push_back({opStr, leftValue, rightValue, tempVar});
        leftValue = tempVar; // Use the temporary variable for further computation
    }

    return leftValue;
}

string parseTerm() {
    string leftValue = parseFactor();

    while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
        TokenType op = tokens[pos].type;
        string opStr = (op == T_MUL) ? "*" : "/";

        pos++;
        string rightValue = parseFactor();
        string tempVar = generateTempVar();

        // Add a TAC instruction
        tacInstructions.push_back({opStr, leftValue, rightValue, tempVar});
        leftValue = tempVar; // Use the temporary variable for further computation
    }

    return leftValue;
}


string parseFactor() {
    if (tokens[pos].type == T_NUM || tokens[pos].type == T_FLOAT) {
        return tokens[pos++].value;  
    } else if (tokens[pos].type == T_ID) {
        string varName = tokens[pos].value;
        if (symbolTable.find(varName) != symbolTable.end()) {
            pos++;
            return varName; 
        } else {
            cout << "Error: Undeclared variable " << varName << " on line " << tokens[pos].line << endl;
            exit(1);
        }
    } else if (tokens[pos].type == T_CHAR || tokens[pos].type == T_STRING) {
        return tokens[pos++].value; 
    } else if (tokens[pos].type == T_LPAREN) {
        expect(T_LPAREN);
        string value = parseExpression();
        expect(T_RPAREN);
        return value;  
    } else {
        cout << "Syntax error: unexpected token " << tokens[pos].value << " on line " << tokens[pos].line << endl;
        exit(1);
    }
}



    void expect(TokenType type) {
        if (tokens[pos].type == type) {
            pos++;
        } else {
            cout << "Syntax error: expected token " << type << " but found " << tokens[pos].value << " on line " << tokens[pos].line << endl;
            exit(1);
        }
    }

    string evaluateExpression(const string &left, const string &right, TokenType op) {
        bool isFloat = (left.find('.') != string::npos || right.find('.') != string::npos);
        double leftVal = stod(left);
        double rightVal = stod(right);

        if (op == T_PLUS) {
            return isFloat ? to_string(leftVal + rightVal) : to_string(static_cast<int>(leftVal + rightVal));
        }
        else if (op == T_MINUS) {
            return isFloat ? to_string(leftVal - rightVal) : to_string(static_cast<int>(leftVal - rightVal));
        }
        else if (op == T_MUL) {
            return isFloat ? to_string(leftVal * rightVal) : to_string(static_cast<int>(leftVal * rightVal));
        }
        else if (op == T_DIV) {
            return isFloat ? to_string(leftVal / rightVal) : to_string(static_cast<int>(leftVal / rightVal));
        }
        else if (op == T_GT) {
            return to_string(stoi(left) > stoi(right));
        }
        else if (op == T_LT) {
            return to_string(stoi(left) < stoi(right));
        }
        else if (op == T_EQ) {
            return to_string(left == right);
        }
        else if (op == T_NEQ) {
            return to_string(left != right);
        }
        else if (op == T_AND) {
            return to_string(static_cast<bool>(leftVal) && static_cast<bool>(rightVal));
        }
        else if (op == T_OR) {
            return to_string(static_cast<bool>(leftVal) || static_cast<bool>(rightVal));
        }
        return ""; 
    }
};
void printTAC() {
    cout << "Three Address Code (TAC):\n";
    for (const auto &inst : tacInstructions) {
        cout << setw(10) << inst.result << " = " << setw(10) << inst.arg1;
        if (!inst.arg2.empty()) {
            cout << " " << inst.op << " " << setw(10) << inst.arg2;
        } else {
            cout << " " << inst.op;
        }
        cout << endl;
    }
}


void printTableWithBorders() {
    // Print table with borders
    const string borderLine = "--------------------------------------------------------------------";
    const string headerLine = "| Variable            | Type          | Value         | Line       |";

    cout << CYAN << borderLine << RESET << endl;
    cout << BLUE << headerLine << RESET << endl;
    cout << CYAN << borderLine << RESET << endl;

    for (const auto &pair : symbolTable) {
        const auto &symbol = pair.second;
        cout << "| " << setw(18) << left << pair.first
             << "| " << setw(13) << left << symbol.type
             << "| " << setw(13) << left << symbol.value
            << "| " << setw(10) << symbol.line << RESET << " |" << endl;
    }

    cout << CYAN << borderLine << RESET << endl;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Usage: " << argv[0] << " <source file>" << endl;
        return 1;
    }

    ifstream file(argv[1]);
    if (!file.is_open()) {
        cout << "Failed to open file: " << argv[1] << endl;
        return 1;
    }

    string src((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    file.close();

    Lexer lexer(src);
    vector<Token> tokens = lexer.tokenize();

    Parser parser(tokens);
    parser.parseProgram();

    printTableWithBorders();
    printTAC();

    return 0;
}