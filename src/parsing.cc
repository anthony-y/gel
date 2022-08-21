#include "ast.h"
#include "lexing.h"
#include "common.h"
#include "arrays.h"

#include <stdarg.h>
#include <stdio.h>
#include <assert.h>

constexpr int AST_MAX_DEPTH = 16;

struct Parsing {
    Array<UntypedCode> ast;
    int ast_stack_top;
    
    TokenData *token;
    int error_count;
};

static void parser_error(Parsing *state, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    // The weird looking escape characters are to: set the text color to red, print "Error", and then reset the colour.
    fprintf(stderr, "%s:%lu: \033[0;31mSyntax error\033[0m: ", "placeholder.gel", 0);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);

    state->error_count++;
}

static bool match_token(Parsing *state, TokenType type) {
    if (state->token->type == type) {
        state->token++;
        return true;
    }
    return false;
}

static void init_ast(UntypedCode *a) {
    array_init(&a->var_decls, 32);
    array_init(&a->func_decls, 32);
    array_init(&a->all_statements, 32);
    array_init(&a->top_directives, 16);
    bucket_array_init(&a->nested_expressions);
}

static UntypedBlockHandle push_ast(Parsing *state) {
    int slot = array_append(&state->ast, UntypedCode {});
    init_ast(&state->ast[slot]);
    state->ast_stack_top++;
    assert(state->ast_stack_top <= AST_MAX_DEPTH);
    return slot;
}

static inline void pop_ast(Parsing *state) {
    state->ast_stack_top--;
}

static UntypedCode *temp_ptr_to_this_ast(Parsing *state) {
    return &state->ast[state->ast_stack_top];
}

static UntypedMatch parse_match(Parsing *state);
static UntypedIf    parse_if(Parsing *state);

static UntypedExpr parse_simple_expression(Parsing *state);
static UntypedExpr parse_expression_list(Parsing *state);
static UntypedExpr parse_assignment(Parsing *state);
static UntypedExpr parse_logical_or(Parsing *state);
static UntypedExpr parse_logical_and(Parsing *state);
static UntypedExpr parse_equality_comparison(Parsing *state);
static UntypedExpr parse_lt_gt_comparison(Parsing *state);
static UntypedExpr parse_addition_subtraction(Parsing *state);
static UntypedExpr parse_multiplication(Parsing *state);
static UntypedExpr parse_division_module(Parsing *state);
static UntypedExpr parse_selector(Parsing *state);
static UntypedExpr parse_postfix(Parsing *state);
static UntypedExpr parse_function_call(Parsing *state, UntypedExpr name);
static UntypedExpr parse_array_view(Parsing *state, UntypedExpr name);

static UntypedExpr parse_expression(Parsing *state) {
    return parse_expression_list(state);
}

static UntypedExpr parse_expression_list(Parsing *state) {
    UntypedExpr e = parse_assignment(state);
    return e;
}

static UntypedExpr parse_assignment(Parsing *state) {

    auto e = parse_logical_or(state);

    // TODO PLUSEQUAL, STAREQUAL, etc.
    while (match_token(state, EQUAL)) {

        auto right = parse_logical_or(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = EQUAL; // TODO here

        e = a;
    }

    return e;
}

static UntypedExpr parse_logical_or(Parsing *state) {

    auto e = parse_logical_and(state);

    while (match_token(state, DOUBLEBAR)) {

        auto right = parse_logical_and(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = DOUBLEBAR;

        e = a;
    }

    return e;
}

static UntypedExpr parse_logical_and(Parsing *state) {

    auto e = parse_equality_comparison(state);

    while (match_token(state, DOUBLEAMPERSAND)) {

        auto right = parse_equality_comparison(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = DOUBLEAMPERSAND;

        e = a;
    }

    return e;
}

static UntypedExpr parse_equality_comparison(Parsing *state) {

    auto e = parse_lt_gt_comparison(state);

    while (match_token(state, EQUALEQUAL)) {

        auto right = parse_lt_gt_comparison(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = EQUALEQUAL;

        e = a;
    }

    return e;
}

static UntypedExpr parse_lt_gt_comparison(Parsing *state) {

    auto e = parse_addition_subtraction(state);

    while (match_token(state, LESSTHAN)) {

        auto right = parse_addition_subtraction(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = LESSTHAN;

        e = a;
    }

    return e;
}

static UntypedExpr parse_addition_subtraction(Parsing *state) {

    auto e = parse_multiplication(state);

    while (match_token(state, PLUS)) {

        auto right = parse_multiplication(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = PLUS;

        e = a;
    }

    return e;
}

static UntypedExpr parse_multiplication(Parsing *state) {

    auto e = parse_division_module(state);

    while (match_token(state, STAR)) {

        auto right = parse_division_module(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = STAR;

        e = a;
    }

    return e;
}

static UntypedExpr parse_division_module(Parsing *state) {

    auto e = parse_selector(state);

    while (match_token(state, SLASH)) {

        auto right = parse_selector(state);

        if (right.tag == EXPR_NONE)
            return right;
        
        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = SLASH;

        e = a;
    }

    return e;
}

// TODO: this may need to be of higher precedence.
static UntypedExpr parse_selector(Parsing *state) {
    auto e = parse_postfix(state);

    while (match_token(state, DOT)) {
        auto right = parse_selector(state);

        if (right.tag == EXPR_NONE)
            return right;

        UntypedExpr a;
        a.tag = EXPR_BINARY;
        a.binary.left = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);
        a.binary.right = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, right);
        a.binary.op = DOT;

        e = a;
    }
    return e;
}

static UntypedExpr parse_postfix(Parsing *state) {
    
    auto e = parse_simple_expression(state);

    while (true) {
        if (match_token(state, LPAREN)) {
            return parse_function_call(state, e);
        }

        if (match_token(state, LBRACKET)) {
            return parse_array_view(state, e);
        }

        break;
    }

    return e;
}

static UntypedExpr parse_function_call(Parsing *state, UntypedExpr name) {

    UntypedExpr e;
    e.tag = EXPR_FUNCTION_CALL;
    e.function_call.name = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, name);

    if (match_token(state, RPAREN)) {
        e.function_call.given_arguments = {nullptr, 0, 0};
    }

    else {
        array_init(&e.function_call.given_arguments, 8);
        array_append(&e.function_call.given_arguments, parse_expression(state));
        while (match_token(state, COMMA)) {
            array_append(&e.function_call.given_arguments, parse_expression(state));
        }
        if (!match_token(state, RPAREN)) {
            parser_error(state, "expected ')'");
            return {};
        }
    }

    return e;
}

static UntypedExpr parse_array_view(Parsing *state, UntypedExpr name) {
    UntypedExpr e;
    e.tag = EXPR_ARRAY_VIEW;
    e.array_view.name = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, name);
    e.array_view.from = nullptr;
    e.array_view.to = nullptr;
    return e;
}

static UntypedExpr parse_simple_expression(Parsing *state) {

    TokenData now = *state->token;
    switch (now.type) {    

        case IDENTIFIER: {
            state->token++;
            return UntypedExpr {
                .tag = EXPR_IDENTIFIER,
                .identifier = Buffer { .data = now.start, .length = (usize)now.length }
            };
        } break;

        case HASH: {
            state->token++;
            auto e = parse_expression(state);
            return UntypedExpr {
                .tag = EXPR_DIRECTIVE,
                .directive = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e)
            };
        } break;

        case INTEGER_LITERAL: {
            state->token++;
            return UntypedExpr {
                .tag = EXPR_INT_LITERAL,
                .int_literal = atol(now.start)
            };
        } break;

        case STRING_LITERAL: {
            state->token++;
            return UntypedExpr {
                .tag = EXPR_STRING_LITERAL,
                .string_literal = Buffer { .data = now.start, .length = (usize)now.length }
            };
        } break;

        case LBRACKET: {
            state->token++;
            auto tmp = parse_expression(state);
            auto size_or_typename = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, tmp);

            if (!match_token(state, SEMICOLON)) {

                if (!match_token(state, RBRACKET)) {
                    parser_error(state, "view types should look like this: [int]");
                    return {};
                }

                return UntypedExpr {
                    .tag = EXPR_VIEW_TYPE,
                    .view_typename = size_or_typename
                };
            }

            auto base_typename = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, parse_expression(state));

            if (!match_token(state, RBRACKET)) {
                parser_error(state, "array types should look like this: [10;int]");
                return {};
            }

            return UntypedExpr {
                .tag = EXPR_ARRAY_TYPE,
                .array_type = UntypedArrayTypeExpr {
                    .size = size_or_typename,
                    .base_typename = base_typename
                }
            };
        } break;

        case LPAREN: {
            state->token++;
            auto inner = parse_expression(state);
            if (!match_token(state, RPAREN)) {
                parser_error(state, "expected ')'");
                return {};
            }
            return UntypedExpr {
                .tag = EXPR_PARENS,
                .parens = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, inner)
            };
        } break;

        case MATCH: {
            state->token++;
            auto m = parse_match(state); 
            return UntypedExpr {
                .tag = EXPR_MATCH,
                .match_clause = m
            };
        } break;

        case FLOAT_LITERAL: {
            state->token++;
            return UntypedExpr {
                .tag = EXPR_FLOAT_LITERAL,
                .float_literal = atof(now.start)
            };
        } break;
    }

    return {};
}

// TODO maybe remove is_const, and just have two separate arrays for let and const decls.
static void parse_var_decl(Parsing *state, bool is_const) {
    
    UntypedDecl<UntypedVar> decl;
    Buffer name = { .data = state->token->start, .length = (usize)state->token->length };
    decl.name = name;

    if (!match_token(state, IDENTIFIER)) {
        parser_error(state, "declaration has no name");
        return;
    }

    decl.data.given_type = parse_selector(state);

    if (match_token(state, EQUAL)) {
        auto expr = parse_expression(state);
        if (expr.tag == EXPR_NONE) {
            parser_error(state, "'%.*s' has no value (did you forget to delete the '='?)", name.length, name.data);
            return;
        }
        decl.data.expr = expr;
    }

    // TODO consider that this might be zero'd anyway
    else {
        decl.data.expr = UntypedExpr { .tag = EXPR_NONE };
    }

    match_token(state, SEMICOLON);

    array_append(&temp_ptr_to_this_ast(state)->var_decls, decl);
}

static UntypedIf parse_if(Parsing *state) {
    return UntypedIf {};
}

static UntypedMatch parse_match(Parsing *state) {
    match_token(state, MATCH);

    auto e = parse_expression(state);
    if (e.tag == EXPR_NONE)
        assert(false);

    UntypedMatch m;
    m.expr = bucket_array_append(&temp_ptr_to_this_ast(state)->nested_expressions, e);

    // TODO memory leak
    array_init(&m.patterns, 6);

    assert(match_token(state, LBRACE));

    while (!match_token(state, RBRACE)) {
        auto pattern = parse_expression(state);
        assert(pattern.tag == EXPR_IDENTIFIER || pattern.tag == EXPR_FUNCTION_CALL);
        assert(match_token(state, BIG_ARROW));
        auto then = parse_expression(state);
        array_append(&m.patterns, UntypedMatcher {
            .pattern = pattern,
            .then = then,
        });
    }

    return m;
}

static void parse_statement(Parsing *state) {
    switch (state->token->type) {
        case CONST:
            state->token++;
            parse_var_decl(state, true);
            break;

        case LET:
            state->token++;
            parse_var_decl(state, false);
            break;

        case MATCH:
            state->token++;
            array_append(&temp_ptr_to_this_ast(state)->all_statements, UntypedExpr {
                .tag = EXPR_MATCH,
                .match_clause = parse_match(state)
            });
            break;

        default: {
            auto e = parse_expression(state);
            match_token(state, SEMICOLON);
            array_append(&temp_ptr_to_this_ast(state)->all_statements, e);
        } break;
    }
}

static void parse_func_decl(Parsing *state) {

    match_token(state, FUNC);

    UntypedDecl<UntypedFunc> decl;
    
    if (state->token->type == IDENTIFIER) {
        decl.name = { .data = state->token->start, .length = (usize)state->token->length };
    } else {
        decl.name = { .data = nullptr, .length = 0 };
    }

    state->token++;

    decl.data.block_handle = push_ast(state);

    assert(match_token(state, LPAREN));

    if (!match_token(state, RPAREN)) {

        parse_var_decl(state, false);

        while (match_token(state, COMMA)) {
            parse_var_decl(state, false);
        }

        assert(match_token(state, RPAREN));
    }

    if (match_token(state, ARROW)) {

        auto first = parse_expression(state);
        if (first.tag == EXPR_NONE) {
            parser_error(state, "expected return type(s)");
            return;
        }

        // TODO Leak
        array_init(&decl.data.given_return_types, 5);

        array_append(&decl.data.given_return_types, first);

        while (match_token(state, COMMA)) {
            array_append(&decl.data.given_return_types, parse_expression(state));
        }
    }

    else {
        decl.data.given_return_types.length = 0;
        decl.data.given_return_types.capacity = 0;
        decl.data.given_return_types.data = nullptr;
    }

    assert(match_token(state, LBRACE));

    while (!match_token(state, RBRACE)) {
        parse_statement(state);
    }

    pop_ast(state);

    array_append(&temp_ptr_to_this_ast(state)->func_decls, decl);
}

void parse_top(Parsing *state, bool *done) {
    TokenData token = *state->token++;
    switch (token.type) {
        case LET:
            parse_var_decl(state, /*is_const=*/false);
            break;

        case CONST:
            parse_var_decl(state, /*is_const=*/true);
            break;

        case FUNC:
            parse_func_decl(state);
            break;

        case HASH:
            array_append(&temp_ptr_to_this_ast(state)->top_directives, parse_expression(state));
            break;

        case END:
            *done = true;
            break;

        default:
            assert(false);
            break;
    }
}

int do_parsing(Array<TokenData> of, Array<UntypedCode> *output) {

    Parsing state;
    state.token = of.data;
    state.error_count = 0;
    state.ast_stack_top = -1;
    array_init(&state.ast, 16);

    push_ast(&state);

    bool done = false;

    while (!done && state.error_count < 1) {
        parse_top(&state, &done);
    }

    *output = state.ast;

    return (state.error_count == 0 ? 0 : -1);
}
