#pragma once

#include "common.h"
#include "lexing.h"
#include "arrays.h"

struct UntypedExpr;

using UntypedBlockRef = int;

enum UntypedThingTag {
    NODE_NONE,

    NODE_VAR_DECL,
    NODE_FUNC_DECL,
    NODE_COERCE_DECL,
    NODE_VARIANT_DECL,
    NODE_DIRECTIVE,

    NODE_IDENTIFIER,

    NODE_BLOCK_START,
    NODE_BLOCK_END,

    NODE_GENERIC_EXPRESSION,
    NODE_TYPE_NAME_EXPRESSION,
    NODE_UNARY_EXPRESSION,
    NODE_BINARY_EXPRESSION,
    NODE_INT_LITERAL_EXPRESSION,
    NODE_STRING_LITERAL_EXPRESSION,
    NODE_FLOAT_LITERAL_EXPRESSION,
    NODE_IDENTIFIER_EXPRESSION,
};

enum UntypedExprTag {
    EXPR_NONE,
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_IDENTIFIER,
    EXPR_TYPENAME,
    EXPR_INT_LITERAL,
    EXPR_STRING_LITERAL,
    EXPR_FLOAT_LITERAL,
    EXPR_FUNCTION_CALL,
    EXPR_ARRAY_VIEW,
    EXPR_VIEW_TYPE,
    EXPR_ARRAY_TYPE,
    EXPR_DIRECTIVE,
    EXPR_MATCH,
    EXPR_IF,
    EXPR_PARENS,
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

struct UntypedMatchExpr {
    UntypedExpr *expr;
    UntypedBlockRef block_ref;
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

        Buffer identifier;
        Buffer string_literal;
        s64 int_literal;
        f64 float_literal;
    };
};

enum UntypedStmtTag {
    STMT_EXPR,
};

struct UntypedStmt {
    UntypedStmtTag tag;
    union {
        UntypedExpr expr;
    };
};

template<typename T> struct UntypedDecl {
    Buffer name;
    T data;
};

struct UntypedVar {
    int flags = 0;
    UntypedExpr expr;
    UntypedExpr given_type;
};

struct UntypedFunc {
    Array<UntypedExpr> given_return_types;
    UntypedBlockRef block_ref;
};

struct UntypedVariant {
};

/* TODO
 * Consider that every block underneath the top-level one will have different types here.
 * It will probably be good in the long run to have an additional structure called, like,
 * "LocalBlock" or something to represent this.
 *
 * But for now, I will just keep this.
*/
struct UntypedCode {
    Array<UntypedDecl<UntypedVar>>  var_decls;
    Array<UntypedDecl<UntypedFunc>> func_decls;
    Array<UntypedStmt> all_statements;
    Array<UntypedExpr> top_directives;

    // TODO these might be better of as normal Array<>s.
    // I will have to see how the access pattern ends up looking.
    BucketArray<UntypedExpr> nested_expressions;
};

int do_parsing(Array<TokenData> of, Array<UntypedCode> *ast);

struct UntypedFile {
    int slot;
    Array<TokenData> token_data;
    Array<UntypedCode> ast;
    Buffer file_data;
};
