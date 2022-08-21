#pragma once

#include "common.h"
#include "lexing.h"
#include "arrays.h"

struct UntypedExpr;
struct UntypedMatcher;

using UntypedBlockHandle = int;

enum UntypedExprTag {
    EXPR_NONE,
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_IDENTIFIER,
    EXPR_INT_LITERAL,
    EXPR_STRING_LITERAL,
    EXPR_FLOAT_LITERAL,
    EXPR_FUNCTION_CALL,
    EXPR_ARRAY_VIEW,
    EXPR_VIEW_TYPE,
    EXPR_ARRAY_TYPE,
    EXPR_DIRECTIVE,
    EXPR_IF,
    EXPR_MATCH,
    EXPR_MATCHER,
    EXPR_PARENS,
    EXPR_USING,
};

struct UntypedBinaryExpr {
    UntypedExpr *left;
    UntypedExpr *right;
    TokenType op;
};

struct UntypedUnaryExpr {
    TokenType op;
    UntypedExpr *inner;
};

struct UntypedFunctionCallExpr {
    UntypedExpr *name;
    Array<UntypedExpr> given_arguments;
};

struct UntypedArrayViewExpr {
    UntypedExpr *name;
    UntypedExpr *from;
    UntypedExpr *to;
};

struct UntypedArrayTypeExpr {
    UntypedExpr *size;
    UntypedExpr *base_typename;
};

struct UntypedMatch {
    UntypedExpr *expr;
    Array<UntypedMatcher> patterns;
};

struct UntypedIf {
    UntypedExpr *condition;
    UntypedExpr *else_clause;
    UntypedBlockHandle block_handle;
};

struct UntypedExpr {
    UntypedExprTag tag;
    union {
        UntypedUnaryExpr unary;
        UntypedBinaryExpr binary;
        UntypedFunctionCallExpr function_call;
        UntypedArrayViewExpr array_view;
        UntypedArrayTypeExpr array_type;

        UntypedExpr *view_typename;
        UntypedExpr *parens;
        UntypedExpr *directive;

        UntypedMatch match_clause;
        UntypedIf if_clause;

        Buffer identifier;
        Buffer string_literal;
        s64 int_literal;
        f64 float_literal;
    };
};

// Pattern matcher inside match clause
struct UntypedMatcher {
    UntypedExpr pattern;
    UntypedExpr then;
};

// Unlike expressions, declarations are not stored contiguously.
// This means they can be of different sizes.
// They will later generate entries in the type-table and/or symbol-table.
template<typename T> struct UntypedDecl {
    BufferView name;
    T data;
};

struct UntypedVar {
    int flags = 0;
    UntypedExpr expr;
    UntypedExpr given_type;
};

struct UntypedFunc {
    Array<UntypedExpr> given_return_types;
    UntypedBlockHandle block_handle;
};

struct UntypedVariant {
};

struct UntypedStruct {
};

struct UntypedCode {
    Array<UntypedDecl<UntypedVar>>  var_decls;
    Array<UntypedDecl<UntypedFunc>> func_decls;
    Array<UntypedExpr> all_statements;
    Array<UntypedExpr> top_directives;

    BucketArray<UntypedExpr> nested_expressions;
};

int do_parsing(Array<TokenData> of, Array<UntypedCode> *ast);

struct UntypedFile {
    int slot;
    Array<TokenData> token_data;
    Array<UntypedCode> ast;
    Buffer file_data;
};
