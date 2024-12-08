#include <iostream>
#include <fstream>
#include <regex>
#include <string>
#include <unordered_set>
#include <vector>

using namespace std;

// Enum for token types
enum TokenType {
    KEYWORD,
    IDENTIFIER,
    NUMBER,
    STRING_LITERAL,
    CHAR_LITERAL,
    SPECIAL_CHAR,
    PREPROCESSOR,
    COMMENT,
    UNKNOWN
};

// Token class to store token type and value
class Token {
public:
    TokenType tokenType;
    string value;

    Token(TokenType type, const string &val) : tokenType(type), value(val) {}

    void printToken() const {
        cout << "<" << tokenTypeToString(tokenType) << ": " << value << ">" << endl;
    }

private:
    string tokenTypeToString(TokenType type) const {
        switch (type) {
            case KEYWORD: return "KEYWORD";
            case IDENTIFIER: return "IDENTIFIER";
            case NUMBER: return "NUMBER";
            case STRING_LITERAL: return "STRING_LITERAL";
            case CHAR_LITERAL: return "CHAR_LITERAL";
            case SPECIAL_CHAR: return "SPECIAL_CHAR";
            case PREPROCESSOR: return "PREPROCESSOR";
            case COMMENT: return "COMMENT";
            default: return "UNKNOWN";
        }
    }
};

// Keywords set
unordered_set<string> keywords = {
    "int", "float", "double", "char", "string", "if", "else", "while", "for", "return", "include", "define", "std", "main"
};

// Function to tokenize the code
vector<Token> tokenize(const string &code) {
    vector<Token> tokens;
    regex tokenPatterns(R"(".*?"|'.*?'|#\s*[^\n]*|\/\/.*?$|\/\*.*?\*\/|\b\d+(\.\d+)?([eE][+-]?\d+)?\b|0[xX][0-9a-fA-F]+|\b[a-zA-Z_]\w*\b|[(){}[\];:,.<>+=\-*/%#&|^!])");

    auto matches_begin = sregex_iterator(code.begin(), code.end(), tokenPatterns);
    auto matches_end = sregex_iterator();

    for (sregex_iterator i = matches_begin; i != matches_end; ++i) {
        smatch match = *i;

        string value = match.str();
        TokenType type = UNKNOWN;

        if (value[0] == '#') { // Preprocessor directive
            type = PREPROCESSOR;
        } else if (value[0] == '/' && (value[1] == '/' || value[1] == '*')) { // Comment
            type = COMMENT;
        } else if (keywords.count(value)) { // Keyword
            type = KEYWORD;
        } else if (regex_match(value, regex(R"(\d+(\.\d+)?([eE][+-]?\d+)?|0[xX][0-9a-fA-F]+)"))) { // Number
            type = NUMBER;
        } else if (regex_match(value, regex(R"([a-zA-Z_]\w*)"))) { // Identifier
            type = IDENTIFIER;
        } else if (value[0] == '"' || value[0] == '\'') { // String or char literal
            type = value.length() > 1 && value[0] == '"' ? STRING_LITERAL : CHAR_LITERAL;
        } else if (regex_match(value, regex(R"([(){}[\];:,.<>+=\-*/%#&|^!])"))) { // Special char
            type = SPECIAL_CHAR;
        }

        tokens.emplace_back(type, value);

        // Debug print
        cout << "Tokenized: <" << value << ">, Type: " << type << endl;
    }

    return tokens;
}


// Function to print tokens
void printTokens(const vector<Token> &tokens) {
    for (const auto &token : tokens) {
        token.printToken();
    }
}

// Main function to read the source code and tokenize
int main() {
    string filePath = "testcode.txt";
    ifstream inputFile(filePath);

    if (!inputFile) {
        cerr << "Error: Could not open file " << filePath << endl;
        return 1;
    }

    cout << "File opened successfully.\n";

    string code((istreambuf_iterator<char>(inputFile)), istreambuf_iterator<char>());
    inputFile.close();

    // cout << "File content:\n" << code << endl;

    vector<Token> tokens = tokenize(code);
    cout << "Tokenization complete. Printing tokens...\n";

    printTokens(tokens);

    return 0;
}
