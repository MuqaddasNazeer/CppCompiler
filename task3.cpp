#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <stdexcept>
#include <cctype>

using namespace std;

enum TokenType
{
    T_INT,
    T_LSHIFT,
    T_RSHIFT,
    T_LE,
    T_FLOAT,
    T_STRING,
    T_CHAR,
    T_BOOL,
    T_DOUBLE,
    T_ID,
    T_NUM,
    T_IF,
    T_ELSE,
    T_RETURN,
    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_GT,
    T_LT,
    T_EOF,
    T_COUT,
    T_CIN,
    T_WHILE,
    T_FOR,
    T_EQ,
    T_NEQ,
    T_INC,
    T_DEC,
    T_LEQ,
    T_GEQ,
    T_AND,
    T_OR,
    T_NOT,
    T_AND_ASSIGN,
};

struct Token
{
    TokenType type;
    string value;
    int lineNumber;
    // Constructor to initialize the Token
    // Constructor to initialize the Token
    Token(TokenType type, const string &value, int lineNumber)
        : type(type), value(value), lineNumber(lineNumber) {}
};

class Lexer
{
private:
    string src;
    size_t pos;
    int lineNumber;

public:
    Lexer(const string &src) : src(src), pos(0), lineNumber(1) {}

    vector<Token> tokenize()
    {
        vector<Token> tokens;
        while (pos < src.size())
        {
            char current = src[pos];

            // Skip newlines and increase line number
            if (current == '\n')
            {
                lineNumber++;
                pos++;
                continue;
            }

            if (current == '#')
            {
                while (pos < src.size() && src[pos] != '\n')
                {
                    pos++; // Skip to the end of the line
                }
                lineNumber++;
                continue;
            }

            if (current == 'u' && src.substr(pos, 6) == "using ")
            {
                while (pos < src.size() && src[pos] != '\n')
                {
                    pos++; // Skip to the end of the line
                }
                lineNumber++;
                continue;
            }

            // Skip whitespaces
            if (isspace(current))
            {
                pos++; // Skip whitespace
                continue;
            }

            // Handle single-line comments (//)
            if (current == '/' && pos + 1 < src.size() && src[pos + 1] == '/')
            {
                while (pos < src.size() && src[pos] != '\n')
                    pos++;    // Skip until the end of the line.
                lineNumber++; // Account for the newline.
                continue;
            }

            // Handle multi-line comments (/* */)
            if (current == '/' && pos + 1 < src.size() && src[pos + 1] == '*')
            {
                pos += 2; // Skip "/*"
                while (pos + 1 < src.size() && !(src[pos] == '*' && src[pos + 1] == '/'))
                {
                    if (src[pos] == '\n')
                        lineNumber++;
                    pos++;
                }
                pos += 2; // Skip "*/"
                continue;
            }

            // Skip preprocessor directives (e.g., #include, #define)
            if (current == '#' && (pos == 0 || src[pos - 1] == '\n'))
            {
                while (pos < src.size() && src[pos] != '\n')
                    pos++;    // Skip until the end of the line.
                lineNumber++; // Account for the newline.
                continue;
            }

            if (current == '<' && pos + 1 < src.size() && src[pos + 1] == '<')
            {
                tokens.push_back(Token{T_COUT, "<<", lineNumber}); // Change T_LSHIFT to T_COUT
                pos += 2;
                continue;
            }

            if (current == '>' && pos + 1 < src.size() && src[pos + 1] == '>')
            {
                tokens.push_back(Token{T_CIN, ">>", lineNumber}); // Change T_RSHIFT to T_CIN
                pos += 2;
                continue;
            }

            // Handle increment (++) and decrement (--)
            if (current == '+' && pos + 1 < src.size() && src[pos + 1] == '+')
            {
                tokens.push_back(Token{T_INC, "++", lineNumber});
                pos += 2; // Skip both '+'
                continue;
            }

            if (current == '-' && pos + 1 < src.size() && src[pos + 1] == '-')
            {
                tokens.push_back(Token{T_DEC, "--", lineNumber});
                pos += 2; // Skip both '-'
                continue;
            }
            // Handle '<' and '>' operators including '<=' and '>='
            if (current == '<')
            {
                if (pos + 1 < src.size() && src[pos + 1] == '=')
                {
                    tokens.push_back(Token{T_LEQ, "<=", lineNumber});
                    pos += 2; // Skip both '<' and '='
                    continue;
                }
                else
                {
                    tokens.push_back(Token{T_LT, "<", lineNumber});
                    pos++; // Skip the '<'
                    continue;
                }
            }

            if (current == '>')
            {
                if (pos + 1 < src.size() && src[pos + 1] == '=')
                {
                    tokens.push_back(Token{T_GEQ, ">=", lineNumber});
                    pos += 2; // Skip both '>' and '='
                    continue;
                }
                else
                {
                    tokens.push_back(Token{T_GT, ">", lineNumber});
                    pos++; // Skip the '>'
                    continue;
                }
            }

            // Handle "==" (Equal to)
            if (current == '=' && pos + 1 < src.size() && src[pos + 1] == '=')
            {
                tokens.push_back(Token{T_EQ, "==", lineNumber});
                pos += 2; // Skip both '=' characters
                continue;
            }

            // Handle "!=" (Not equal to)
            if (current == '!' && pos + 1 < src.size() && src[pos + 1] == '=')
            {
                tokens.push_back(Token{T_NEQ, "!=", lineNumber});
                pos += 2; // Skip both '!' and '=' characters
                continue;
            }

            if (current == '"')
            {
                // Handle string literal
                pos++;                // Skip the opening quote
                string strValue = ""; // Initialize a variable to store the string value
                while (pos < src.size() && src[pos] != '"')
                {
                    if (src[pos] == '\\' && pos + 1 < src.size() && (src[pos + 1] == '"' || src[pos + 1] == '\\'))
                    {
                        // Handle escaped quotes and backslashes (e.g., \" or \\ inside a string)
                        strValue += src[pos + 1]; // Add the escaped character
                        pos += 2;                 // Skip the escape character and the following special character
                    }
                    else if (src[pos] == '\\')
                    {
                        // Handle other escape sequences if needed
                        cout << "Error: Invalid escape sequence at line " << lineNumber << endl;
                        exit(1);
                    }
                    else
                    {
                        strValue += src[pos]; // Append the character to the string value
                        pos++;                // Move to the next character
                    }
                }

                if (pos < src.size() && src[pos] == '"')
                {
                    pos++;                                                   // Skip the closing quote
                    tokens.push_back(Token{T_STRING, strValue, lineNumber}); // Store the string value
                    continue;                                                // Proceed to the next character
                }
                else
                {
                    // Handle error for unmatched quote
                    cout << "Error: Unmatched double quote at line " << lineNumber << endl;
                    exit(1);
                }
            }

            // Handle numbers
            if (isdigit(current))
            {
                tokens.push_back(Token{T_NUM, consumeNumber(), lineNumber});
                continue;
            }

            // Handle identifiers and keywords (e.g., int, float, if, else, etc.)

            if (isalpha(current) || current == '_')
            {
                string word = consumeWord();
                TokenType type = T_ID;

                // Check for reserved keywords
                if (word == "cout")
                    type = T_COUT;
                else if (word == "while") // Add 'while' here
                    type = T_WHILE;
                else if (word == "cin")
                    type = T_CIN;
                else if (word == "int")
                    type = T_INT;
                else if (word == "float")
                    type = T_FLOAT;
                else if (word == "double")
                    type = T_DOUBLE;
                else if (word == "bool")
                    type = T_BOOL;
                else if (word == "char")
                    type = T_CHAR;
                else if (word == "string")
                    type = T_STRING;
                else if (word == "if")
                    type = T_IF;
                else if (word == "else")
                    type = T_ELSE;
                else if (word == "for")
                    type = T_FOR;
                else if (word == "return")
                    type = T_RETURN;

                tokens.push_back(Token{type, word, lineNumber});
                continue;
            }

            // Handle symbols such as '=', '+', '*', etc.
            switch (current)
            {
            case '=':
                tokens.push_back(Token{T_ASSIGN, "=", lineNumber});
                break;
            case '+':
                tokens.push_back(Token{T_PLUS, "+", lineNumber});
                break;
            case '-':
                tokens.push_back(Token{T_MINUS, "-", lineNumber});
                break;
            case '*':
                tokens.push_back(Token{T_MUL, "*", lineNumber});
                break;
            case '/':
                tokens.push_back(Token{T_DIV, "/", lineNumber});
                break;
            case '(':
                tokens.push_back(Token{T_LPAREN, "(", lineNumber});
                break;
            case ')':
                tokens.push_back(Token{T_RPAREN, ")", lineNumber});
                break;
            case '{':
                tokens.push_back(Token{T_LBRACE, "{", lineNumber});
                break;
            case '}':
                tokens.push_back(Token{T_RBRACE, "}", lineNumber});
                break;
            case ';':
                tokens.push_back(Token{T_SEMICOLON, ";", lineNumber});
                break;
            default:
                cout << "Unexpected character: " << current << " at line " << lineNumber << endl;
                exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", lineNumber});

        // Print tokens as they are generated
        for (const auto &token : tokens)
        {
            cout << "Token: " << tokenTypeToString(token.type)
                 << ", Value: " << token.value
                 << ", Line: " << token.lineNumber << endl;
        }

        return tokens;
    }

    string consumeNumber()
    {
        size_t start = pos;
        bool hasDot = false;

        if (src[pos] == '-')
            pos++; // Allow a leading negative sign

        while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.'))
        {
            if (src[pos] == '.')
            {
                if (hasDot) // If a second dot is found, break
                    break;
                hasDot = true;
            }
            pos++;
        }

        string number = src.substr(start, pos - start);

        // Validate the number format
        if (hasDot && (number[0] == '.' || number.back() == '.'))
        {
            cout << "Error: Invalid number format '" << number << "' at line " << lineNumber << endl;
            exit(1);
        }

        return number;
    }

    string consumeWord()
    {
        size_t start = pos;
        while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_'))
            pos++;
        return src.substr(start, pos - start);
    }

private:
    // Helper function to convert TokenType to string
    string tokenTypeToString(TokenType type)
    {
        switch (type)
        {
        case T_INT:
            return "T_INT";
        case T_FLOAT:
            return "T_FLOAT";
        case T_STRING:
            return "T_STRING";
        case T_CHAR:
            return "T_CHAR";
        case T_BOOL:
            return "T_BOOL";
        case T_DOUBLE:
            return "T_DOUBLE";
        case T_ID:
            return "T_ID";
        case T_NUM:
            return "T_NUM";
        case T_IF:
            return "T_IF";
        case T_ELSE:
            return "T_ELSE";
        case T_RETURN:
            return "T_RETURN";
        case T_ASSIGN:
            return "T_ASSIGN";
        case T_PLUS:
            return "T_PLUS";
        case T_MINUS:
            return "T_MINUS";
        case T_MUL:
            return "T_MUL";
        case T_DIV:
            return "T_DIV";
        case T_LPAREN:
            return "T_LPAREN";
        case T_RPAREN:
            return "T_RPAREN";
        case T_LBRACE:
            return "T_LBRACE";
        case T_RBRACE:
            return "T_RBRACE";
        case T_SEMICOLON:
            return "T_SEMICOLON";
        case T_GT:
            return "T_GT";
        case T_LT:
            return "T_LT";
        case T_WHILE:
            return "T_WHILE";
        case T_FOR:
            return "T_FOR";
        case T_COUT:
            return "T_COUT";
        case T_CIN:
            return "T_CIN";
        case T_EOF:
            return "T_EOF";
        case T_INC:
            return "T_INC";
        case T_DEC:
            return "T_DEC";
        case T_LEQ:
            return "T_LEQ";
        case T_GEQ:
            return "T_GEQ";
        default:
            return "UNKNOWN";
        }
    }
};

class TypeInfo
{
public:
    enum class Type
    {
        INT,
        FLOAT,
        DOUBLE,
        BOOL,
        CHAR,
        STRING,
        VOID,
        FUNCTION,
        ARRAY
    };
    Type type;
    vector<Type> paramTypes;
    // For functions, store parameter types

    // Constructor for regular types
    TypeInfo(Type t) : type(t) {}

    // Constructor for function types (with parameter types)
    TypeInfo(Type t, vector<Type> params) : type(t), paramTypes(params) {}

    // For displaying type names as strings
    string toString() const
    {
        switch (type)
        {
        case Type::INT:
            return "int";
        case Type::FLOAT:
            return "float";
        case Type::DOUBLE:
            return "double";
        case Type::BOOL:
            return "bool";
        case Type::CHAR:
            return "char";
        case Type::STRING:
            return "string";
        case Type::VOID:
            return "void";
        case Type::FUNCTION:
            return "function";
        case Type::ARRAY:
            return "array";
        default:
            return "unknown";
        }
    }

    // Check if the TypeInfo is equivalent to another (used for function matching)
    bool equals(const TypeInfo &other) const
    {
        if (type != other.type)
            return false;
        if (paramTypes.size() != other.paramTypes.size())
            return false;
        for (size_t i = 0; i < paramTypes.size(); ++i)
        {
            if (paramTypes[i] != other.paramTypes[i])
                return false;
        }
        return true;
    }
};

class SymbolTableEntry
{
public:
    string name;
    TypeInfo type;
    bool isFunction;
    bool isArray;

    SymbolTableEntry(string n, TypeInfo t, bool func = false, bool arr = false)
        : name(n), type(t), isFunction(func), isArray(arr) {}
};

// Symbol Table
class SymbolTable
{
public:
    void declareVariable(const string &name, const string &type)
    {
        if (symbolTable.find(name) != symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name + "' is already declared.");
        }
        symbolTable[name] = type;
    }

    string getVariableType(const string &name)
    {
        if (symbolTable.find(name) == symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name + "' is not declared.");
        }
        return symbolTable[name];
    }

    // No need for "SymbolTable::" here, just declare the functions normally.
    bool isDeclared(const string &name) const
    {
        return symbolTable.find(name) != symbolTable.end();
    }

    void declareType(const string &name, const string &category)
    {
        if (symbolTable.find(name) != symbolTable.end())
        {
            throw runtime_error("Semantic error: Type '" + name + "' is already declared.");
        }
        symbolTable[name] = category;
    }

    void printSymbolTable() const
    {
        cout << "Symbol Table: " << endl;
        for (const auto &entry : symbolTable)
        {
            cout << "Variable: " << entry.first << ", Type: " << entry.second << endl;
        }
    }

private:
    map<string, string> symbolTable;
};

// Intermediate Code Generator
class IntermediateCodeGnerator
{
private:
    int labelCount;              // Counter to generate unique labels
    int tempCount;               // Counter to generate unique temporary variables
    vector<string> instructions; // Stores the generated instructions

public:
    IntermediateCodeGnerator() : labelCount(0), tempCount(0) {}

    // Generate a new unique label
    string newLabel()
    {
        return "L" + to_string(labelCount++);
    }

    // Generate a new unique temporary variable
    string newTemp()
    {
        return "t" + to_string(tempCount++);
    }

    // Add an instruction to the intermediate code
    void addInstruction(const string &instruction)
    {
        instructions.push_back(instruction);
    }

    // Print all the generated instructions
    void printInstructions()
    {
        for (const string &instr : instructions)
        {
            cout << instr << endl;
        }
    }
};

// Parser
class Parser
{
public:
    Parser(const vector<Token> &tokens, SymbolTable &symTable, IntermediateCodeGnerator &icg)
        : tokens(tokens), pos(0), symTable(symTable), icg(icg) {}

    void parseProgram()
    {
        while (tokens[pos].type != T_EOF)
        {
            if (tokens[pos].type == T_INT) // Assuming functions start with a return type
            {
                parseFunction();
            }
            else
            {
                parseStatement();
            }
        }
    }

private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable &symTable;
    IntermediateCodeGnerator &icg;

    void parseStatement()
    {
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT || tokens[pos].type == T_DOUBLE || tokens[pos].type == T_BOOL || tokens[pos].type == T_CHAR || tokens[pos].type == T_STRING)
        {
            parseDeclaration();
        }
        else if (tokens[pos].type == T_ID)
        {
            // Handle increment/decrement operation if followed by T_INC or T_DEC
            if (tokens[pos + 1].type == T_INC || tokens[pos + 1].type == T_DEC || tokens[pos + 1].type == T_LEQ || tokens[pos + 1].type == T_GT)
            {
                parseIncrementDecrement(); // Handle increment/decrement
            }
            else
            {
                parseAssignment(); // Regular assignment
            }
        }

        else if (tokens[pos].type == T_IF)
        {
            parseIfStatement();
        }
        else if (tokens[pos].type == T_RETURN)
        {
            parseReturnStatement();
        }
        else if (tokens[pos].type == T_COUT)
        {
            parseCoutStatement();
        }
        else if (tokens[pos].type == T_CIN)
        {
            parseCinStatement();
        }
        else if (tokens[pos].type == T_WHILE)
        {
            parseWhileStatement(); // Handle while loop
        }
        else if (tokens[pos].type == T_FOR)
        {
            parseForStatement(); // Handle for loop
        }
        else if (tokens[pos].type == T_LBRACE)
        {
            parseBlock();
        }
        else
        {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }
    void parseDeclaration()
    {
        TokenType type = tokens[pos].type;
        if (type == T_INT || type == T_FLOAT || type == T_DOUBLE || type == T_BOOL || type == T_CHAR || type == T_STRING)
        {
            pos++;
        }
        else
        {
            cout << "Syntax error: Expected a data type at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }

        string varName = expectAndReturnValue(T_ID);
        symTable.declareVariable(varName, tokenTypeToString(type));
        expect(T_SEMICOLON);
    }

    void parseIncrementDecrement()
    {
        string id = parseIdentifier(); // Parse the variable name (e.g., i)

        // Check for increment or decrement operator
        if (tokens[pos].type == T_INC || tokens[pos].type == T_DEC)
        {
            TokenType opType = tokens[pos].type;

            if (opType == T_INC)
            {
                icg.addInstruction(id + " = " + id + " + 1"); // Increment the variable
            }
            else if (opType == T_DEC)
            {
                icg.addInstruction(id + " = " + id + " - 1"); // Decrement the variable
            }

            pos++;               // Consume the increment/decrement operator (T_INC or T_DEC)
            expect(T_SEMICOLON); // Expect a semicolon after the operation
        }
        else
        {
            cout << "Error: Expected increment or decrement operator after identifier" << endl;
            exit(1); // Exit on error, can be improved with better error handling
        }
    }

    void parseAssignment()
    {
        string varName = expectAndReturnValue(T_ID);
        symTable.getVariableType(varName);
        expect(T_ASSIGN);
        string expr = parseExpression();
        icg.addInstruction(varName + " = " + expr);
        expect(T_SEMICOLON);
    }
    void parseWhileStatement()
    {
        expect(T_WHILE);  // Expect 'while'
        expect(T_LPAREN); // Expect '('

        // Parse the condition expression
        string cond = parseExpression(); // Parse the loop condition

        expect(T_RPAREN); // Expect ')'

        // Generate unique labels for the loop
        string startLabel = icg.newLabel();     // Label for the start of the loop
        string conditionLabel = icg.newLabel(); // Label for condition check
        string endLabel = icg.newLabel();       // Label for exiting the loop

        // Start loop logic
        icg.addInstruction("goto " + conditionLabel); // Jump to condition check
        icg.addInstruction("L" + startLabel + ":");

        // Parse the loop body
        if (tokens[pos].type == T_LBRACE)
        {
            expect(T_LBRACE); // Expect '{'
            while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
            {
                parseStatement(); // Parse statements inside the loop body
            }
            expect(T_RBRACE); // Expect '}'
        }
        else
        {
            parseStatement(); // Parse a single statement as loop body
        }

        // Jump back to the condition
        icg.addInstruction("goto L" + conditionLabel);

        // Condition check
        icg.addInstruction("L" + conditionLabel + ":");
        string temp = icg.newTemp();
        icg.addInstruction(temp + " = " + cond);
        icg.addInstruction("if " + temp + " goto L" + startLabel); // Loop again if condition is true

        // End of the loop
        icg.addInstruction("L" + endLabel + ":");
    }

    void parseForStatement()
    {
        cout << "Parsing for loop..." << endl;

        // Expect 'for'
        expect(T_FOR);
        cout << "Token 'for' processed." << endl;

        // Expect '('
        expect(T_LPAREN);
        cout << "Token '(' processed." << endl;

        // Parse initialization part: j = 0
        parseStatement(); // This will parse the initialization part (e.g., j = 0)
        cout << "Initialization statement parsed." << endl;

        // Expect ';' after initialization
        expect(T_SEMICOLON);
        cout << "Semicolon after initialization processed." << endl;

        // *Debugging Step*: Check the next token after initialization and semicolon
        cout << "Next token after initialization semicolon: "
             << tokenTypeToString(tokens[pos].type) << " with value '"
             << tokens[pos].value << "'" << endl;

        // Parse the condition: j < 10
        string condition = parseCondition(); // Parse the condition explicitly
        cout << "Condition parsed: " << condition << endl;

        // Expect ';' after condition
        expect(T_SEMICOLON);
        cout << "Semicolon after condition processed." << endl;

        // *Debugging Step*: Check the next token after condition and semicolon
        cout << "Next token after condition semicolon: "
             << tokenTypeToString(tokens[pos].type) << " with value '"
             << tokens[pos].value << "'" << endl;

        // Parse increment: j++
        parseIncrementDecrement(); // This will handle increment (j++) or (i++) or any other form of increment
        cout << "Increment parsed." << endl;

        // Expect ')' after increment
        expect(T_RPAREN);
        cout << "Closing parenthesis processed." << endl;

        // Generate Intermediate Code for the loop
        string startLabel = icg.newTemp();
        string endLabel = icg.newTemp();

        icg.addInstruction("L" + startLabel + ":");

        // Evaluate condition and jump if false
        string temp = icg.newTemp();
        icg.addInstruction(temp + " = " + condition);
        icg.addInstruction("if " + temp + " goto L" + endLabel); // Jump if false

        // Parse the body of the loop
        parseStatement(); // Apply the body of the loop

        // Loop back to the start
        icg.addInstruction("goto L" + startLabel);

        // End label for the loop
        icg.addInstruction("L" + endLabel + ":");
    }

    string parseCondition()
    {
        string lhs = parseExpression(); // Left-hand side (e.g., j)
        string op = tokens[pos].value;  // Operator (e.g., <)
        expect(T_LT);                   // Or expect the appropriate comparison operator
        string rhs = parseExpression(); // Right-hand side (e.g., 10)
        return lhs + " " + op + " " + rhs;
    }

    string parseExpression()
    {
        string term = parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS)
        {
            TokenType op = tokens[pos++].type;
            string nextTerm = parseTerm();
            string temp = icg.newTemp();
            icg.addInstruction(temp + " = " + term + (op == T_PLUS ? " + " : " - ") + nextTerm);
            term = temp;
        }

        // Now handle relational operators like '>', '<', '>=', '<=', '==', '!='
        if (tokens[pos].type == T_GT || tokens[pos].type == T_LT ||
            tokens[pos].type == T_GEQ || tokens[pos].type == T_LEQ ||
            tokens[pos].type == T_EQ || tokens[pos].type == T_NEQ)
        {
            TokenType op = tokens[pos++].type;
            string nextExpr = parseExpression(); // Parse the next part of the expression
            string temp = icg.newTemp();

            // Handle the relational operator in the intermediate code
            if (op == T_GT)
                icg.addInstruction(temp + " = " + term + " > " + nextExpr);
            else if (op == T_LT)
                icg.addInstruction(temp + " = " + term + " < " + nextExpr);
            else if (op == T_GEQ)
                icg.addInstruction(temp + " = " + term + " >= " + nextExpr);
            else if (op == T_LEQ)
                icg.addInstruction(temp + " = " + term + " <= " + nextExpr);
            else if (op == T_EQ)
                icg.addInstruction(temp + " = " + term + " == " + nextExpr);
            else if (op == T_NEQ)
                icg.addInstruction(temp + " = " + term + " != " + nextExpr);

            term = temp;
        }

        return term;
    }
    void parseFunction()
    {
        expect(T_INT); // Assuming function returns int (can extend for other types)
        expect(T_ID);  // Function name
        expect(T_LPAREN);
        expect(T_RPAREN);
        expect(T_LBRACE);

        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement(); // Parse function body statements
        }
        expect(T_RBRACE);
    }

    string parseTerm()
    {
        string factor = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV)
        {
            TokenType op = tokens[pos++].type;
            string nextFactor = parseFactor();
            string temp = icg.newTemp();
            icg.addInstruction(temp + " = " + factor + (op == T_MUL ? " * " : " / ") + nextFactor);
            factor = temp;
        }
        return factor;
    }

    string parseFactor()
    {
        if (tokens[pos].type == T_NUM)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_ID)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_LPAREN)
        {
            expect(T_LPAREN);
            string expr = parseExpression();
            expect(T_RPAREN);
            return expr;
        }
        else
        {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }

    void parseCoutStatement()
    {
        expect(T_COUT); // Expect 'cout'

        // Parse the first expression after <<
        expect(T_COUT); // Expect '<<'

        // Loop to handle multiple << operators and values
        while (true)
        {
            if (tokens[pos].type == T_STRING)
            {
                string strValue = expectAndReturnValue(T_STRING);
                icg.addInstruction("cout << \"" + strValue + "\"");
            }
            else if (tokens[pos].type == T_ID)
            {
                string idValue = expectAndReturnValue(T_ID);
                icg.addInstruction("cout << " + idValue);
            }
            else
            {
                break; // Break if no valid expression is found after <<
            }

            if (tokens[pos].type == T_COUT)
            {
                expect(T_COUT); // Expect next '<<' token
            }
            else
            {
                break; // If there's no more '<<', end the statement
            }
        }

        // Ensure we are at the semicolon to end the statement
        if (tokens[pos].type == T_SEMICOLON)
        {
            expect(T_SEMICOLON); // Expect the semicolon at the end of the statement
        }
        else
        {
            // Handle case where a semicolon is missing, or provide a more specific error message.
            std::cerr << "Syntax Error: Expected semicolon at the end of the statement.\n";
            exit(1); // Or return some appropriate error code
        }

        // Now the parser should be ready to handle the next statement.
        // This could be an if, cin, or another statement type.
        if (tokens[pos].type == T_CIN || tokens[pos].type == T_IF)
        {
            // Handle the next token appropriately (could call another function for cin/if parsing)
        }
    }

    void parseCinStatement()
    {
        expect(T_CIN); // Expect 'cin'

        expect(T_CIN); // Expect the >> operator

        // Parse the variable after >>
        if (tokens[pos].type == T_ID)
        {
            string varName = expectAndReturnValue(T_ID);
            icg.addInstruction("cin >> " + varName);
        }
        else
        {
            std::cerr << "Syntax Error: Expected variable after cin >>.\n";
            exit(1); // Or handle appropriately
        }

        // Ensure we have a semicolon at the end of the statement
        if (tokens[pos].type == T_SEMICOLON)
        {
            expect(T_SEMICOLON); // Expect the semicolon at the end of the cin statement
        }
        else
        {
            std::cerr << "Syntax Error: Expected semicolon at the end of the cin statement.\n";
            exit(1);
        }
    }

    string tokenTypeToString(TokenType type)
    {
        switch (type)
        {
        case T_INT:
            return "int";
        case T_FLOAT:
            return "float";
        case T_DOUBLE:
            return "double";
        case T_BOOL:
            return "bool";
        case T_CHAR:
            return "char";
        case T_STRING:
            return "string";
        default:
            return "";
        }
    }

    void parseIfStatement()
    {
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

        if (tokens[pos].type == T_ELSE)
        {
            icg.addInstruction("goto L3");
            icg.addInstruction("L2:");
            expect(T_ELSE);
            parseStatement();
            icg.addInstruction("L3:");
        }
        else
        {
            icg.addInstruction("L2:");
        }
    }

    void parseReturnStatement()
    {
        expect(T_RETURN);
        string expr = parseExpression();
        icg.addInstruction("return " + expr);
        expect(T_SEMICOLON);
    }

    void parseBlock()
    {
        expect(T_LBRACE);
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement();
        }
        expect(T_RBRACE);
    }

    void expect(TokenType expectedType)
    {
        if (tokens[pos].type != expectedType)
        {
            cout << "Syntax error: expected token " << expectedType << " at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
        else
        {
            // cout << "Expected token '" << expectedType << "' found: '" << tokens[pos].value << "'" << endl;
        }
        pos++;
    }

    string parseIdentifier()
    {
        if (tokens[pos].type != T_ID)
        {
            cout << "Syntax error: expected identifier but found '" << tokens[pos].value << "' at line " << tokens[pos].lineNumber << endl;
            exit(1); // Exit or handle the error appropriately
        }
        string id = tokens[pos].value; // Retrieve the identifier's value
        pos++;                         // Advance to the next token
        return id;
    }

    string expectAndReturnValue(TokenType type)
    {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }
};
class AssemblyCodeGenerator
{
private:
    int labelCount;
    int tempCount;
    vector<string> instructions;
    map<string, string> varToReg; // To map variables to registers (or memory locations)
    vector<string> assemblyCode;  // Stores the generated assembly code

public:
    AssemblyCodeGenerator() : labelCount(0), tempCount(0) {}

    string newLabel()
    {
        return "L" + to_string(labelCount++);
    }

    string newTemp()
    {
        return "t" + to_string(tempCount++);
    }

    // Add intermediate instruction to assembly
    void addInstruction(const string &instruction)
    {
        instructions.push_back(instruction);
    }

    // Map variable to a register
    string getRegisterForVar(const string &var)
    {
        if (varToReg.find(var) == varToReg.end())
        {
            // Allocate a new register for the variable if not already mapped
            varToReg[var] = "R" + to_string(varToReg.size());
        }
        return varToReg[var];
    }

    // Generate assembly code for a single instruction
    void generateAssembly()
    {
        for (const string &instr : instructions)
        {
            // Example mapping of operations
            if (instr.find("=") != string::npos)
            {
                // Assignment: var = expression
                size_t pos = instr.find("=");
                string var = instr.substr(0, pos - 1);
                string expr = instr.substr(pos + 2);

                string reg = getRegisterForVar(var);

                // For now, assume expr is a direct number or variable, more complex expressions can be parsed
                if (expr[0] >= '0' && expr[0] <= '9')
                {
                    // If the expression is a number
                    assemblyCode.push_back("MOV " + reg + ", " + expr);
                }
                else
                {
                    // If the expression is a variable
                    string exprReg = getRegisterForVar(expr);
                    assemblyCode.push_back("MOV " + reg + ", " + exprReg);
                }
            }
            else if (instr.find("if") != string::npos)
            {
                // Handle if statements
                size_t pos = instr.find("if");
                string condition = instr.substr(3); // Get condition part
                string labelTrue = newLabel();
                string labelFalse = newLabel();

                // Assuming the condition is just a comparison like "a >= b"
                assemblyCode.push_back("CMP " + condition); // Simplified for now
                assemblyCode.push_back("JE " + labelTrue);
                assemblyCode.push_back("JMP " + labelFalse);
                assemblyCode.push_back(labelTrue + ":");
            }
            else if (instr.find("while") != string::npos)
            {
                // Handle while loops
                size_t pos = instr.find("while");
                string condition = instr.substr(6); // Get condition part
                string labelStart = newLabel();
                string labelEnd = newLabel();

                assemblyCode.push_back(labelStart + ":");
                assemblyCode.push_back("CMP " + condition); // Simplified for now
                assemblyCode.push_back("JE " + labelEnd);
                assemblyCode.push_back("JMP " + labelStart);
                assemblyCode.push_back(labelEnd + ":");
            }
        }
    }

    // Print the generated assembly code
    void printAssembly()
    {
        for (const string &line : assemblyCode)
        {
            cout << line << endl;
        }
    }
};

int main()
{
    string src = R"(
    #include <iostream>
    #include <math.h>
    #include <stdio.h>
    using namespace std;

    int main() {
        int x;
        float y;
        bool isActive;
        int a;
        a = 15;
        int b;
        b = 10;
        c=5;
        if (a >= b) {
            a = b;
        }
        else {
            b = a;
        }
        string name;
        // Single line comment
        /*multiline comments
        sfsafasf
        */
        x = 10;
        y = 3.14;
        isActive = true;

        int sum;
        cout << "Hello, world!" << endl;
        cin >> x;
        sum = x + y * 3;

        if (x > 3) {
            x = 20;
        }

        int i;
        i = 10;
        while (i <= 12) {
            i--;
            cout << i;
        }
        return 0;
    }
    )";

    // Initialize Lexer and Tokenize the input
    Lexer lexer(src);
    vector<Token> tokens = lexer.tokenize();

    // Initialize SymbolTable, IntermediateCodeGenerator, and AssemblyCodeGenerator
    SymbolTable symTable;
    IntermediateCodeGnerator icg;
    AssemblyCodeGenerator acg;
    Parser parser(tokens, symTable, icg);

    // Start parsing the program
    parser.parseProgram();

    // Print the symbol table after parsing
    symTable.printSymbolTable(); // This will print all declared variables and their types.

    // Generate and print intermediate code and assembly
    icg.printInstructions();
    acg.generateAssembly();
    acg.printAssembly();

    return 0;
}
