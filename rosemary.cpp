#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <string> 
#include <cctype>
#include <cstdio>
#include <memory>
#include <utility>
#include <vector>
#include <unordered_map>

//------------------------------------------------------------------------------------------------//
// Lexer
//------------------------------------------------------------------------------------------------//

enum Token {
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5
};

static const std::string DEF_KEY = "def";
static const std::string EXTERN_KEY = "extern";
static const char COMMENT_KEY = '#';

static std::string identifier_str;
static double num_val; 

static int get_tok() {
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
        bool dec_flag = false; 
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

//------------------------------------------------------------------------------------------------//
// Abstract Syntax Tree
//------------------------------------------------------------------------------------------------//

class ExprAST {
public:
    virtual ~ExprAST() = default; 
    virtual llvm::Value* codegen() = 0; 
};

class NumberExprAST : public ExprAST {
private:
    double val; 
public: 
    NumberExprAST(double val)
    : val(val) {}

    llvm::Value* codegen() override; 
};

class VariableExprAST : public ExprAST {
private:
    std::string name; 
public: 
    VariableExprAST(std::string name) 
    : name(name) {}

    llvm::Value* codegen() override; 
};

class BinaryExprAST : public ExprAST {
private:
    char op; 
    std::unique_ptr<ExprAST> lhs, rhs; 

public:
    BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    llvm::Value* codegen() override; 
};

class CallExprAST : public ExprAST {
private:
    std::string callee;
    std::vector< std::unique_ptr<ExprAST> > args; 

public:
    CallExprAST(std::string callee, std::vector< std::unique_ptr<ExprAST> > args) 
    : callee(callee), args(std::move(args)) {}

    llvm::Value* codegen() override; 
};

class PrototypeAST {
private:
    std::string name; 
    std::vector<std::string> args; 
public:
    PrototypeAST(const std::string& name, std::vector<std::string> args) 
    : name(name), args(std::move(args)) {}

    const std::string& get_name() const { return name; }

    llvm::Function* codegen();
};

class FunctionAST {
private:
    std::unique_ptr<PrototypeAST> proto;
    std::unique_ptr<ExprAST> body; 

public:
    FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body) 
    : proto(std::move(proto)), body(std::move(body)) {}

    llvm::Function* codegen();
};

//------------------------------------------------------------------------------------------------//
// Parser
//------------------------------------------------------------------------------------------------//

static int cur_tok;
static int get_next_tok() {
    return cur_tok = get_tok();
}

static std::unordered_map<char, int> binop_precedence = {
    {'<', 10},
    {'+', 20},
    {'-', 20},
    {'*', 40}
};

static std::unique_ptr<ExprAST> log_error(const char* str) {
    std::fprintf(stderr, "Error: %s\n", str);
    return nullptr; 
}

static std::unique_ptr<PrototypeAST> log_error_p(const char* str) {
    log_error(str);
    return nullptr; 
}

static int get_tok_precedence() {
    if (!std::isprint(cur_tok)) return -1; 

    int tok_prec = binop_precedence[cur_tok];
    if (tok_prec <= 0) return -1; 
    return tok_prec; 
}

static std::unique_ptr<ExprAST> parse_expression(); 

static std::unique_ptr<ExprAST> parse_number_expr() {
    auto result = std::make_unique<NumberExprAST>(num_val);
    get_next_tok(); // Eat the number. 
    return std::move(result);
}

static std::unique_ptr<ExprAST> parse_paren_expr() {
    get_next_tok(); // Eat the '('.

    auto inner = parse_expression();
    if (!inner) return nullptr; 
    if (cur_tok != ')') return log_error("Expected ')'");
    
    get_next_tok(); // Eat the ')'.
    return inner; 
}

static std::unique_ptr<ExprAST> parse_identifier_expr() {
    std::string id = identifier_str;

    get_next_tok(); // Eat the identifier.

    if (cur_tok != '(') return std::make_unique<VariableExprAST>(id);

    get_next_tok(); // Eat the '('.

    std::vector< std::unique_ptr<ExprAST> > args; 
    if (cur_tok != ')') {
        while (true) {
            if (auto arg = parse_expression()) args.push_back(std::move(arg));
            else return nullptr; 

            if (cur_tok == ')') break; 

            if (cur_tok != ',') return log_error("Expected ')' or ',' in argument list"); 
            get_next_tok(); // Eat the ','. 
        }
    }

    get_next_tok(); // Eat the ')'.

    return std::make_unique<CallExprAST>(id, std::move(args));
}

static std::unique_ptr<ExprAST> parse_primary() {
    switch (cur_tok) {
        case Token::tok_identifier: return parse_identifier_expr();
        case Token::tok_number: return parse_number_expr();
        case '(': return parse_paren_expr();
        default: return log_error("Unknown token when expecting an expression");
    }
}

static std::unique_ptr<ExprAST> parse_bin_op_rhs(int expr_prec, std::unique_ptr<ExprAST> lhs) {
    while (true) {
        int tok_prec = get_tok_precedence();
        // Acts as termination for base function call, otherwise terminates when rhs is fully
        // formed. Call this if statement *. 
        if (tok_prec < expr_prec) return lhs; 

        int bin_op = cur_tok;
        get_next_tok(); // Eat the bin_op. 

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

        // Not higher precedence OR higher precedence but rhs has been fully formed after *.
        // Now make node. 
        lhs = std::make_unique<BinaryExprAST>(bin_op, std::move(lhs), std::move(rhs));
    }
}

static std::unique_ptr<ExprAST> parse_expression() {
    auto lhs = parse_primary();
    if (!lhs) return nullptr; 

    return parse_bin_op_rhs(0, std::move(lhs));
}

static std::unique_ptr<PrototypeAST> parse_prototype() {
    if (cur_tok != Token::tok_identifier) {
        return log_error_p("Expected function name in prototype");
    }

    std::string fn_name = identifier_str;
    get_next_tok(); // Eat the Token::tok_identifier. 

    if (cur_tok != '(') return log_error_p("Expected '(' in prototype");

    std::vector<std::string> arg_names; 
    while (get_next_tok() == tok_identifier) arg_names.push_back(identifier_str);

    if (cur_tok != ')') return log_error_p("Expected ')' in prototype");

    get_next_tok(); // Eat the ')'.

    return std::make_unique<PrototypeAST>(fn_name, std::move(arg_names)); 
}

static std::unique_ptr<FunctionAST> parse_definition() {
    get_next_tok(); // Eat the DEF_KEY. 

    auto proto = parse_prototype();
    if (!proto) return nullptr; 

    if (auto e = parse_expression()) {
        return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
    }
    return nullptr; 
}

static std::unique_ptr<PrototypeAST> parse_extern() {
    get_next_tok(); // Eat the EXTERN_KEY. 
    return parse_prototype();
}

static std::unique_ptr<FunctionAST> parse_top_level_expr() {
    if (auto e = parse_expression()) {
        auto proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
    }
    return nullptr; 
}

//------------------------------------------------------------------------------------------------//
// Code Generation
//------------------------------------------------------------------------------------------------//

static std::unique_ptr<llvm::LLVMContext> the_context; 
static std::unique_ptr< llvm::IRBuilder<> > builder;  
static std::unique_ptr<llvm::Module> the_module;
static std::map<std::string, llvm::Value*> named_values; 

llvm::Value* log_error_v(const char* str) {
    log_error(str);
    return nullptr; 
}

llvm::Value* NumberExprAST::codegen() {
    return llvm::ConstantFP::get(*the_context, llvm::APFloat(val));
}

llvm::Value* VariableExprAST::codegen() {
    llvm::Value* v = named_values[name];
    if (!v) log_error_v("Unknown variable name");
    return v; 
}

llvm::Value* BinaryExprAST::codegen() {
    llvm::Value* left = lhs->codegen();
    llvm::Value* right = rhs->codegen();

    if (!left || !right) return nullptr; 

    switch (op) {
        case '+':
            return builder->CreateFAdd(left, right, "addtmp");
        case '-':
            return builder->CreateFSub(left, right, "subtmp");
        case '*':
            return builder->CreateFMul(left, right, "multmp");
        case '<':
            left = builder->CreateFCmpULT(left, right, "cmptmp");
            return builder->CreateUIToFP(left, llvm::Type::getDoubleTy(*the_context), "booltmp");
        default:
            return log_error_v("Invalid binary operator");
    }
}

llvm::Value* CallExprAST::codegen() {
    llvm::Function* callee_f = the_module->getFunction(callee);

    if (!callee_f) return log_error_v("Unknown function reference");
    if (callee_f->arg_size() != args.size()) return log_error_v("Incorrect number of arguments");

    std::vector<llvm::Value*> args_v; 
    for (unsigned i = 0; i != args.size(); ++i) {
        args_v.push_back(args[i]->codegen());
        if (!args_v.back()) return nullptr; 
    }

    return builder->CreateCall(callee_f, args_v, "calltmp");
}

llvm::Function* PrototypeAST::codegen() {
    std::vector<llvm::Type*> doubles(args.size(), llvm::Type::getDoubleTy(*the_context));

    llvm::FunctionType* ft = llvm::FunctionType::get(
        llvm::Type::getDoubleTy(*the_context), 
        doubles, 
        false
    );

    llvm::Function* f = llvm::Function::Create(
        ft, 
        llvm::Function::ExternalLinkage,
        name, 
        the_module.get()
    );

    // Name arguments 
    unsigned idx = 0;
    for (auto& arg : f->args()) arg.setName(args[idx++]);


    return f; 
}

llvm::Function* FunctionAST::codegen() {
    llvm::Function* the_function = the_module->getFunction(proto->get_name());

    if (!the_function) the_function = proto->codegen();

    if (!the_function) return nullptr; 

    if (!the_function->empty()) return (llvm::Function*)log_error_v("Redefinition of function.");

    llvm::BasicBlock* bb = llvm::BasicBlock::Create(*the_context, "entry", the_function);
    builder->SetInsertPoint(bb);

    named_values.clear();
    for (auto& arg : the_function->args()) named_values[arg.getName()] = &arg;

    if (llvm::Value* ret_val = body->codegen()) {
        builder->CreateRet(ret_val);
        llvm::verifyFunction(*the_function);
        return the_function; 
    }

    the_function->eraseFromParent();
    return nullptr; 
}

//------------------------------------------------------------------------------------------------//
// Top-Level Parsing & JIT Driver
//------------------------------------------------------------------------------------------------//

static void initialize_module() {
    the_context = std::make_unique<llvm::LLVMContext>();
    the_module = std::make_unique<llvm::Module>("Rosemary JIT", *the_context);
    builder = std::make_unique< llvm::IRBuilder<> >(*the_context);
}

static void handle_definition() {
    if (auto fn_ast = parse_definition()) {
        if (auto* fn_ir = fn_ast->codegen()) {
            fprintf(stderr, "Read function definition:\n");
            fn_ir->print(llvm::errs());
            fprintf(stderr, "\n");
        }
    } else {
        get_next_tok(); // Eat the token for error recovery. 
    }
}

static void handle_extern() {
    if (auto proto_ast = parse_extern()) {
        if (auto* fn_ir = proto_ast->codegen()) {
            fprintf(stderr, "Read extern:\n");
            fn_ir->print(llvm::errs());
            fprintf(stderr, "\n");
        }
    } else {
        get_next_tok(); // Eat the token for error recovery.
    }
}

static void handle_top_level_expression() {
    if (auto fn_ast = parse_top_level_expr()) {
        if (auto* fn_ir = fn_ast->codegen()) {
            fprintf(stderr, "Read top-level expression:\n");
            fn_ir->print(llvm::errs());
            fprintf(stderr, "\n");

            // Remove anonymous function. 
            fn_ir->eraseFromParent();
        }
    } else {
        get_next_tok(); // Eat the token for error recovery.
    }
}

static void main_loop() {
    while (true) {
        switch (cur_tok) {
            case Token::tok_eof:
                return; 
            case ';':
                std::fprintf(stderr, ">> ");
                get_next_tok(); // Eat the ';'.
                break;
            case Token::tok_def:
                handle_definition();
                break;
            case Token::tok_extern:
                handle_extern();
                break;
            default:
                handle_top_level_expression();
                break; 
        }
    }
}

//------------------------------------------------------------------------------------------------//
// Main Driver 
//------------------------------------------------------------------------------------------------//

int main() {
    fprintf(stderr, ">> ");
    get_next_tok();

    initialize_module();

    main_loop();

    the_module->print(llvm::errs(), nullptr);

    return 0; 
}
