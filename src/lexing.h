#pragma once

#include "common.h" 
#include "arrays.h"

enum TokenType : int {
    END = 0,
    ERROR,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    DOT,
    COMMA,
    SEMICOLON,
    EXCLAMATION,
    QUESTION,
    DOLLAR,
    PERCENT,
    BAR,
    DOUBLEBAR,
    AMPERSAND,
    DOUBLEAMPERSAND,
    LESSTHAN,
    GREATERTHAN,
    LESSTHANEQUAL,
    GREATERTHANEQUAL,
    STAR,
    AT,
    PLUS,
    MINUS,
    SLASH,
    BACKSLASH,
    HASH,
    ARROW,
    BIG_ARROW,
    EQUAL,
    EQUALEQUAL, // TODO maybe rename to DOUBLEEQUAL
    PLUSEQUAL,
    MINUSEQUAL,
    STAREQUAL,
    SLASHEQUAL,

    STRING_LITERAL,
    INTEGER_LITERAL,
    FLOAT_LITERAL,
    IDENTIFIER,

    LET,
    FOR,

    IF,

    CONST,
    USING,
    MATCH,
    BREAK,

    RETURN,

    CONTINUE,

    FUNC,
    ELSE,
    THEN,
    LOOP,
    WITH,

    TOKEN_TYPES_COUNT,
};

// Should be 16 bytes on 64-bit
struct TokenData {
    TokenType type;
    int length;
    u8 *start;
};

bool do_lexical_analysis(Buffer of, BucketArray<TokenData> *into);
