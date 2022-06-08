#include <string> 
#include <cctype>
#include <cstdio>
#include <memory>
#include <utility>
#include <vector>
#include <unordered_map>

#include <iostream>

std::unordered_map<char, int> binop_precedence = {
    {'<', 10},
    {'+', 20},
    {'-', 20},
    {'*', 40}
};

enum Token {
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5
};

const std::string DEF_KEY = "def";
const std::string EXTERN_KEY = "extern";
const char COMMENT_KEY = '#';

std::string identifier_str;
double num_val; 

int get_tok() {
    static int last_char = ' ';

    while (std::isspace(last_char)) last_char = std::getchar();
    
    if (std::isalpha(last_char)) {
        identifier_str = last_char;

        while (std::isalnum(last_char = std::getchar())) identifier_str += last_char; 

        if (identifier_str == DEF_KEY) return Token::tok_def;
        if (identifier_str == EXTERN_KEY) return Token::tok_extern;
        return Token::tok_identifier; 
    }

    if (std::isdigit(last_char)) {
        bool dec_flag; 
        std::string num_str(1, last_char);

        while (std::isdigit(last_char = std::getchar()) || (last_char == '.' && !dec_flag)) {
            num_str += last_char;
            if (!dec_flag && last_char == '.') dec_flag = !dec_flag; 
        }

        num_val = std::stod(num_str);
        return Token::tok_number; 
    }

    if (last_char == COMMENT_KEY) {
        last_char = std::getchar();
        while (last_char != EOF && last_char != '\n' && last_char != '\r') {
            last_char = std::getchar(); 
        }
        if (last_char != EOF) return get_tok(); 
    }

    if (last_char == EOF) {
        return Token::tok_eof; 
    } 

    int save = last_char; 
    last_char = std::getchar();
    return save; 
}

class ExprAST {
public:
    virtual ~ExprAST() {}
};

class NumberExprAST : public ExprAST {
private:
    double val; 
public: 
    NumberExprAST(double val)
    : val(val) {}
};

class VariableExprAST : public ExprAST {
private:
    std::string name; 
public: 
    VariableExprAST(std::string name) 
    : name(name) {}
};

class BinaryExprAST : public ExprAST {
private:
    char op; 
    std::unique_ptr<ExprAST> lhs, rhs; 

public:
    BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};

class CallExprAST: public ExprAST {
private:
    std::string callee;
    std::vector< std::unique_ptr<ExprAST> > args; 

public:
    CallExprAST(std::string callee, std::vector< std::unique_ptr<ExprAST> > args) 
    : callee(callee), args(std::move(args)) {}
};

class PrototypeAST {
private:
    std::string name; 
    std::vector<std::string> args; 
public:
    PrototypeAST(const std::string& name, std::vector<std::string> args) 
    : name(name), args(std::move(args)) {}

    const std::string& get_name() const { return name; }
};

class FunctionAST {
private:
    std::unique_ptr<PrototypeAST> proto;
    std::unique_ptr<ExprAST> body; 

public:
    FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body) 
    : proto(std::move(proto)), body(std::move(body)) {}
};

int cur_tok;
int get_next_tok() {
    return cur_tok = get_tok();
}

std::unique_ptr<ExprAST> log_error(const char* str) {
    std::fprintf(stderr, "log_error: %s\n", str);
    return nullptr; 
}

std::unique_ptr<PrototypeAST> log_error_p(const char* str) {
    log_error(str);
    return nullptr; 
}

std::unique_ptr<ExprAST> parse_number_expr() {
    auto result = std::make_unique<NumberExprAST>(num_val);
    get_next_tok();
    return std::move(result);
}

std::unique_ptr<ExprAST> parse_paren_expr() {
    get_next_tok();

    auto inner = parse_expression();
    if (!inner) return nullptr; 
    if (cur_tok != ')') return log_error("expected ')'");
    
    get_next_tok();
    return inner; 
}

std::unique_ptr<ExprAST> parse_identifier_expr() {
    std::string id = identifier_str;

    get_next_tok();

    if (cur_tok != '(') return std::make_unique<VariableExprAST>(id);

    get_next_tok();

    std::vector< std::unique_ptr<ExprAST> > args; 
    if (cur_tok != ')') {
        while (true) {
            if (auto arg = parse_expression()) args.push_back(std::move(arg));
            else return nullptr; 

            if (cur_tok == ')') break; 

            if (cur_tok != ',') return log_error("Expected ')' or ',' in argument list"); 
            get_next_tok();
        }
    }


    return std::make_unique<CallExprAST>(id, std::move(args));
}

std::unique_ptr<ExprAST> parse_primary() {
    switch (cur_tok) {
        case Token::tok_identifier: return parse_identifier_expr();
        case Token::tok_number: return parse_number_expr();
        case '(': return parse_paren_expr();
        default: return log_error("unknown token when expecting an expression");
    }
}

int get_tok_precedence() {
    if (!std::isprint(cur_tok)) return -1; 

    int tok_prec = binop_precedence[cur_tok];
    if (tok_prec <= 0) return -1; 
    return tok_prec; 
}

std::unique_ptr<ExprAST> parse_expression() {
    auto lhs = parse_primary();
    if (!lhs) return nullptr; 

    return parse_bin_op_rhs(0, std::move(lhs));
}

std::unique_ptr<ExprAST> parse_bin_op_rhs(int expr_prec, std::unique_ptr<ExprAST> lhs) {
    while (true) {
        int tok_prec = get_tok_precedence();
        // Acts as termination for base function call, otherwise terminates when expression is fully
        // formed. Call this if statement *. 
        if (tok_prec < expr_prec) return lhs; 

        int bin_op = cur_tok;
        get_next_tok();

        // At this line, cur_tok is the first primary expression after bin_op.
        auto rhs = parse_primary();
        if (!rhs) return nullptr; 

        // Make node now? First check if next bin_op has higher precedence. 
        int next_prec = get_tok_precedence();
        if (tok_prec < next_prec) {
            // Continue forming rhs until *. 
            rhs = parse_bin_op_rhs(tok_prec + 1, std::move(rhs));
            if (!rhs) return nullptr; 
        }

        // Not higher precedence OR higher precedence but expression has been fully formed after *.
        // Now make node. 
        lhs = std::make_unique<BinaryExprAST>(bin_op, std::move(lhs), std::move(rhs));
    }
}

int main() {
    while(true) {
        get_tok();
         
        std::cout << (identifier_str.empty() ? "EMPTY" : identifier_str) << std::endl; 
        std::cout << num_val << std::endl; 
        std::cout << std::endl;
    }
    return 0; 
}
