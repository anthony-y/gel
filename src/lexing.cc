#include "lexing.h"
#include "defer.h"
//
#include "arrays.h"
#include "common.h"

struct Lexing {
    u8 *this_byte;
    int error_count = 0;
};

bool is_digit(u8 c) { return (c >= '0' && c <= '9'); }
bool is_alpha(u8 c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
bool is_start_of_ident(u8 c) { return is_alpha(c) || (c == '_'); }
bool is_ident_char(u8 c) { return is_digit(c) || is_start_of_ident(c); }

// TODO real unicode support
int utf8_decode(u8 *from) {
    return *from;
}

static bool match_next(Lexing *state, char c) {
    if (state->this_byte[0] == c) {
        state->this_byte++;
        return true;
    }
    return false;
}

static void skip_whitespace(Lexing *state) {
    while (true) {
        if (*state->this_byte == ' ') state->this_byte++;
        else if (*state->this_byte == '\t') state->this_byte++;
        else if (*state->this_byte == '\n') state->this_byte++;
        else break;
    }

    while (state->this_byte[0] == '/' && state->this_byte[1] == '/') {
        state->this_byte += 2;
        while (*state->this_byte++ != '\n');
        match_next(state, '\n');
    }
}

static TokenType compute_potential_keyword(u8 *start, int length) {

    switch (length) {
    case 3: {
             if (strncmp(start, "let", 3) == 0) return LET;
        else if (strncmp(start, "for", 3) == 0) return FOR;
    } break;

    case 4: {
             if (strncmp(start, "func", 4) == 0) return FUNC;
        else if (strncmp(start, "else", 4) == 0) return ELSE;
        else if (strncmp(start, "then", 4) == 0) return THEN;
        else if (strncmp(start, "loop", 4) == 0) return LOOP;
        else if (strncmp(start, "with", 4) == 0) return WITH;
    } break;

    case 5: {
             if (strncmp(start, "const", 5) == 0) return CONST;
        else if (strncmp(start, "using", 5) == 0) return USING;
        else if (strncmp(start, "match", 5) == 0) return MATCH;
        else if (strncmp(start, "break", 5) == 0) return BREAK;
    } break;
    }

    return IDENTIFIER;
}

static TokenType compute_next_token(Lexing *state, u8 *start, bool *done) {

    int c = utf8_decode(state->this_byte++);

    switch (c) {

        case '(': return LPAREN;        case ')': return RPAREN;
        case '{': return LBRACE;        case '}': return RBRACE;
        case '[': return LBRACKET;      case ']': return RBRACKET;
        case '.': return DOT;           case ',': return COMMA;
        case ';': return SEMICOLON;     case '!': return EXCLAMATION;
        case '?': return QUESTION;      case '$': return DOLLAR;
        case '%': return PERCENT;       case '@': return AT;
        case '#': return HASH;          case '\\': return BACKSLASH;
        
        case '=': return match_next(state, '=') ? EQUALEQUAL : match_next(state, '>') ? BIG_ARROW : EQUAL;
        case '-': return match_next(state, '=') ? MINUSEQUAL : match_next(state, '>') ? ARROW : MINUS;
        case '+': return match_next(state, '=') ? PLUSEQUAL       : PLUS;
        case '*': return match_next(state, '=') ? STAREQUAL       : STAR;
        case '/': return match_next(state, '=') ? SLASHEQUAL      : SLASH;
        case '|': return match_next(state, '|') ? DOUBLEBAR       : BAR;
        case '&': return match_next(state, '&') ? DOUBLEAMPERSAND : AMPERSAND;

        case 0: {
            *done=true;
            return END;
        }

        default:
           break;
    }

    // String literal
    if (c == '"') {
        while (utf8_decode(state->this_byte++) != '"');
        return STRING_LITERAL;
    }

    // Identifier
    if (is_start_of_ident(c)) {
        while (is_ident_char(*state->this_byte)) {
            state->this_byte++;
        }
        return compute_potential_keyword(start, (state->this_byte - start));
    }

    // Integer or float literal
    if (is_digit(c)) {
        bool dot = false;
        while (true) {
            if (*state->this_byte == '.') dot = true;
            else if (*state->this_byte == '_') {}
            else if (!is_digit(*state->this_byte)) break;
            state->this_byte++;
        }

        return (dot ? FLOAT_LITERAL : INTEGER_LITERAL);
    }

    state->error_count++;
    return ERROR;
}

// Performs lexical analysis `of` a UTF-8 encoded string.
// Populates `output` with tokens.
int do_lexical_analysis(Buffer of, BucketArray<TokenData> *output) {

    Lexing state;
    state.this_byte = of.data;

    bool done = false;

    while (!done) {

        skip_whitespace(&state);

        u8 *start = state.this_byte;

        // This will perform a heap allocation every, like, 64 tokens.
        // TODO it can also fail, so we should error and propagate.
        bucket_array_append(output, TokenData {
            .type   = compute_next_token(&state, start, &done),
            .length = (int)(state.this_byte - start),
            .start  = start,
        });
    }

    return state.error_count;
}
