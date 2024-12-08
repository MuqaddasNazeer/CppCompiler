#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <stdexcept>
#include <iomanip>
using namespace std;

// Enumerations for token types
enum TokenType {
    T_INT, T_ID, T_NUM, T_IF, T_ELSE, T_ASSIGN, T_PLUS, T_MINUS, T_MUL,
    T_DIV, T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE, T_SEMICOLON, T_GT, T_LT,
    T_AND, T_OR, T_EQ, T_NEQ, T_WHILE, T_FOR, T_CUSTOM_IF, T_EOF
};

// Struct for tokens
struct Token {
    TokenType type;
    string value;
    int lineNumber;
    int column;
};

// Symbol table class
class SymbolTable {
    map<string, string> symbols;

public:
    void declareVariable(const string &name, const string &type) {
        if (symbols.find(name) != symbols.end())
            throw runtime_error("Variable '" + name + "' already declared.");
        symbols[name] = type;
    }

    string getVariableType(const string &name) {
        if (symbols.find(name) == symbols.end())
            throw runtime_error("Variable '" + name + "' not declared.");
        return symbols[name];
    }

    void printTable() {
        cout << "\nSymbol Table:\n";
        cout << setw(15) << "Variable" << setw(10) << "Type\n";
        for (const auto &entry : symbols)
            cout << setw(15) << entry.first << setw(10) << entry.second << endl;
    }
};

// Lexer class
class Lexer {
    string src;
    size_t pos;
    int lineNumber;
    int column;

public:
    Lexer(const string &src) : src(src), pos(0), lineNumber(1), column(1) {}

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < src.size()) {
            char current = src[pos];

            // Handle line breaks, whitespace, and comments
            if (current == '\n') { lineNumber++; column = 1; pos++; continue; }
            if (isspace(current)) { column++; pos++; continue; }
            if (current == '/' && src[pos + 1] == '/') {
                while (pos < src.size() && src[pos] != '\n') pos++;
                continue;
            }
            if (current == '/' && src[pos + 1] == '*') {
                pos += 2;
                while (pos < src.size() - 1 && !(src[pos] == '*' && src[pos + 1] == '/')) pos++;
                pos += 2;
                continue;
            }

            // Handle numbers
            if (isdigit(current)) {
                tokens.push_back({T_NUM, consumeNumber(), lineNumber, column});
                continue;
            }

            // Handle keywords and identifiers
            if (isalpha(current)) {
                string word = consumeWord();
                TokenType type = T_ID;

                if (word == "int") type = T_INT;
                else if (word == "if" || word == "Agar") type = T_IF;
                else if (word == "else") type = T_ELSE;
                else if (word == "while") type = T_WHILE;
                else if (word == "for") type = T_FOR;

                tokens.push_back({type, word, lineNumber, column});
                continue;
            }

            // Handle operators and special characters
            switch (current) {
                case '=': tokens.push_back({(src[pos + 1] == '=') ? (++pos, T_EQ) : T_ASSIGN, "=", lineNumber, column}); break;
                case '!': tokens.push_back({(src[pos + 1] == '=') ? (++pos, T_NEQ) : T_EOF, "!=", lineNumber, column}); break;
                case '&': if (src[pos + 1] == '&') { pos++; tokens.push_back({T_AND, "&&", lineNumber, column}); } break;
                case '|': if (src[pos + 1] == '|') { pos++; tokens.push_back({T_OR, "||", lineNumber, column}); } break;
                case '<': tokens.push_back({T_LT, "<", lineNumber, column}); break;
                case '>': tokens.push_back({T_GT, ">", lineNumber, column}); break;
                case '+': tokens.push_back({T_PLUS, "+", lineNumber, column}); break;
                case '-': tokens.push_back({T_MINUS, "-", lineNumber, column}); break;
                case '': tokens.push_back({T_MUL, "", lineNumber, column}); break;
                case '/': tokens.push_back({T_DIV, "/", lineNumber, column}); break;
                case '(': tokens.push_back({T_LPAREN, "(", lineNumber, column}); break;
                case ')': tokens.push_back({T_RPAREN, ")", lineNumber, column}); break;
                case '{': tokens.push_back({T_LBRACE, "{", lineNumber, column}); break;
                case '}': tokens.push_back({T_RBRACE, "}", lineNumber, column}); break;
                case ';': tokens.push_back({T_SEMICOLON, ";", lineNumber, column}); break;
                default:
                    cout << "Unexpected character: " << current << " at line " << lineNumber << ", column " << column << endl;
                    exit(1);
            }
            pos++;
            column++;
        }
        tokens.push_back({T_EOF, "", lineNumber, column});
        return tokens;
    }

private:
    string consumeNumber() {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos])) pos++;
        column += pos - start;
        return src.substr(start, pos - start);
    }

    string consumeWord() {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos])) pos++;
        column += pos - start;
        return src.substr(start, pos - start);
    }
};

// Intermediate code generator
class IntermediateCodeGenerator {
    vector<string> instructions;
    int tempCounter = 0;

public:
    string newTemp() { return "t" + to_string(tempCounter++); }

    void addInstruction(const string &instr) { instructions.push_back(instr); }

    void printInstructions() {
        cout << "\nIntermediate Code (Three-Address Code):\n";
        for (const auto &instr : instructions) cout << instr << endl;
    }

    const vector<string> &getInstructions() const { return instructions; }
};

// Assembly code generator
class AssemblyCodeGenerator {
public:
    void generateAssembly(const vector<string> &icgInstructions) {
        cout << "\nAssembly Code:\n";
        for (const auto &instr : icgInstructions) {
            if (instr.find(" = ") != string::npos) {
                size_t eqPos = instr.find(" = ");
                string lhs = instr.substr(0, eqPos);
                string rhs = instr.substr(eqPos + 3);
                if (rhs.find(" + ") != string::npos) {
                    generateBinaryOperation(lhs, rhs, "ADD");
                } else if (rhs.find(" - ") != string::npos) {
                    generateBinaryOperation(lhs, rhs, "SUB");
                } else {
                    cout << "MOV " << lhs << ", " << rhs << endl;
                }
            } else {
                cout << instr << endl;
            }
        }
    }

private:
    void generateBinaryOperation(const string &lhs, const string &rhs, const string &op) {
        size_t opPos = rhs.find(op == "ADD" ? " + " : " - ");
        string operand1 = rhs.substr(0, opPos);
        string operand2 = rhs.substr(opPos + 3);
        cout << "MOV AX, " << operand1 << endl;
        cout << op << " AX, " << operand2 << endl;
        cout << "MOV " << lhs << ", AX" << endl;
    }
};

// Parser class
class Parser {
    vector<Token> tokens;
    size_t pos = 0;
    SymbolTable &symTable;
    IntermediateCodeGenerator &icg;

public:
    Parser(vector<Token> &tokens, SymbolTable &symTable, IntermediateCodeGenerator &icg)
        : tokens(tokens), symTable(symTable), icg(icg) {}

    void parseProgram() {
        while (tokens[pos].type != T_EOF) parseStatement();
    }

private:
    void parseStatement() {
        if (tokens[pos].type == T_INT) {
            parseDeclaration();
        } else if (tokens[pos].type == T_ID) {
            parseAssignment();
        } else if (tokens[pos].type == T_IF) {
            parseIfStatement();
        } else {
            cout << "Syntax error at token: " << tokens[pos].value << " on line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }

    void parseDeclaration() {
        expect(T_INT);
        string varName = expectAndReturnValue(T_ID);
        symTable.declareVariable(varName, "int");
        expect(T_SEMICOLON);
    }

    void parseAssignment() {
        string varName = expectAndReturnValue(T_ID);
        expect(T_ASSIGN);
        string expr = parseExpression();
        icg.addInstruction(varName + " = " + expr);
        expect(T_SEMICOLON);
    }

    void parseIfStatement() {
        expect(T_IF);
        expect(T_LPAREN);
        string cond = parseExpression();
        expect(T_RPAREN);

        string temp = icg.newTemp();
        icg.addInstruction(temp + " = " + cond);
        icg.addInstruction("if " + temp + " goto L1");
        icg.addInstruction("goto L2");
        icg.addInstruction("L1:");
        parseStatement();
        icg.addInstruction("L2:");
    }

    string parseExpression() {
        string term = parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS) {
            TokenType op = tokens[pos++].type;
            string nextTerm = parseTerm();
            string temp = icg.newTemp();
            icg.addInstruction(temp + " = " + term + (op == T_PLUS ? " + " : " - ") + nextTerm);
            term = temp;
        }
        return term;
    }

    string parseTerm() {
        string factor = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
            TokenType op = tokens[pos++].type;
            string nextFactor = parseFactor();
            string temp = icg.newTemp();
            icg.addInstruction(temp + " = " + factor + (op == T_MUL ? " * " : " / ") + nextFactor);
            factor = temp;
        }
        return factor;
    }

    string parseFactor() {
        if (tokens[pos].type == T_NUM) {
            return tokens[pos++].value;
        } else if (tokens[pos].type == T_ID) {
            return tokens[pos++].value;
        } else {
            cout << "Syntax error: Unexpected token gggg '" << tokens[pos].value << "'" << endl;
            exit(1);
        }
    }

    void expect(TokenType type) {
        if (tokens[pos].type != type) {
            cout << "Syntax error: Expected token '" << type << "', got '" << tokens[pos].value << "'" << endl;
            exit(1);
        }
        pos++;
    }

    string expectAndReturnValue(TokenType type) {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }
};

// Main compiler logic
int main() {
    // Read source code from file
    ifstream file("sample_input.txt");
    if (!file.is_open()) {
        cerr << "Error: Unable to open source file.\n";
        return 1;
    }
    string src((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    file.close();

    // Step 1: Lexical Analysis
    Lexer lexer(src);
    vector<Token> tokens = lexer.tokenize();

    cout << "\n--- Tokens (Lexical Analysis) ---\n";
    for (const auto &token : tokens) {
        cout << "Type: " << token.type << ", Value: " << token.value
             << ", Line: " << token.lineNumber << ", Column: " << token.column << endl;
    }

    // Initialize components
    SymbolTable symTable;
    IntermediateCodeGenerator icg;
    Parser parser(tokens, symTable, icg);

    try {
        // Step 2: Parsing and Intermediate Code Generation
        cout << "\n--- Parsing and Intermediate Code Generation ---\n";
        parser.parseProgram();

        // Step 3: Display Symbol Table
        cout << "\n--- Symbol Table ---\n";
        symTable.printTable();

        // Step 4: Display Intermediate Code
        cout << "\n--- Intermediate Code ---\n";
        icg.printInstructions();

        // Step 5: Generate and Display Assembly Code
        cout << "\n--- Assembly Code ---\n";
        AssemblyCodeGenerator asmGen;
        asmGen.generateAssembly(icg.getInstructions());
    } catch (const runtime_error &e) {
        cerr << "\nError during compilation: " << e.what() << endl;
    }

    return 0;
}