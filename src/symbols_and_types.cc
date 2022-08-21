#include "symbols_and_types.h"
#include "common.h"
#include "arrays.h"
#include "table.h"
#include "ast.h"
#include "defer.h"

// (temp) printf
#include <stdio.h>

#include <string_view>

#define Error_Type() (TypeHandle { .flags = 0, .slot = -1})
#define Symbol_As_Type(the_slot) (TypeHandle { .flags = 0, .slot = the_slot})

struct Typing {
    const Array<UntypedCode> from_ast;
    TypedFile into;
};

Buffer copy_decl_name(BufferView n) {
    u8 *data = (u8 *)malloc(n.length+1);
    strncpy(data, n.data, n.length);
    return { .data = data, .length = n.length };
}

constexpr Buffer copy_string(std::string_view raw) {
    usize length = raw.size();
    auto data = (u8 *)malloc(length+1);
    strcpy(data, raw.data());
    data[length] = 0;
    return { .data = data, .length = length };
}

TypeHandle resolve_expression_as_typename(Typing *state, UntypedExpr type_name) {

    switch (type_name.tag) {

    case EXPR_NONE: return Error_Type();

    case EXPR_BINARY: return Error_Type();

    case EXPR_FLOAT_LITERAL: return Error_Type();

    case EXPR_ARRAY_TYPE: return Error_Type();

    case EXPR_DIRECTIVE: {
        auto inner_handle = resolve_expression_as_typename(state, *type_name.directive);
        TypeHandle wrapped;
        wrapped.flags = inner_handle.flags | TYPE_IS_COMPILE_TIME;
        wrapped.slot = inner_handle.slot;
        return wrapped;
    } break;

    case EXPR_IDENTIFIER: {
        
        Buffer identifier = type_name.identifier;
        Result<TypedDeclHandle> decl_handle = table_get(state->into.symbol_table, identifier);

        if (decl_handle.tag == Error) {
            printf("no such type '%.*s'\n", identifier.length, identifier.data);
            return Error_Type();
        }

        return Symbol_As_Type(decl_handle.ok.slot);

    } break;

    }

    return Error_Type();
}

TypeHandle produce_function_type(Typing *state, UntypedFunc f, Buffer copied_name) {
    return Error_Type();
}

TypeHandle produce_variant_type(Typing *state, UntypedVariant v, Buffer copied_name) {
    return Error_Type();
}

TypeHandle produce_struct_type(Typing *state, UntypedStruct s, Buffer copied_name) {
    return Error_Type();
}

TypeHandle compute_type_of_expression(Typing *state, UntypedExpr of) {

    switch (of.tag) {

    case EXPR_INT_LITERAL: {   
        auto result = table_get(state->into.symbol_table, copy_string("int"));
        assert(result.tag == Ok);
        assert(result.ok.tag == DECL_TYPE);

        TypeHandle handle;
        handle.slot = result.ok.slot;
        handle.flags |= TYPE_IS_COMPILE_TIME;
        return handle;

    } break;

    case EXPR_STRING_LITERAL: {
        auto result = table_get(state->into.symbol_table, copy_string("string"));
        assert(result.tag == Ok);
        assert(result.ok.tag == DECL_TYPE);

        TypeHandle handle;
        handle.slot = result.ok.slot;
        handle.flags |= TYPE_IS_COMPILE_TIME;
        return handle;

    } break;

    }

    return Error_Type();
}

ScopeHandle apply_types_to_owned_block(Typing *state, UntypedBlockHandle block_handle) {
    UntypedCode block = state->from_ast.data[block_handle];

    Scope resulting;
    array_init(&resulting.locals, 16); // LEAK
    array_init(&resulting.statements, 16); // LEAK
    resulting.slot = state->into.all_scopes.length;
    array_append(&state->into.all_scopes, resulting);

    for (int i = 0; i < block.all_statements.length; i++) {
        auto s = block.all_statements.data[i];
    }

    for (int i = 0; i < block.var_decls.length; i++) {
        auto v = block.var_decls.data[i];
    }

    return resulting.slot;
}

Typed<FunctionDecl> function_decl(Typing *state, UntypedDecl<UntypedFunc> f) {
    Typed<FunctionDecl> decl;
    decl.name = copy_decl_name(f.name); // LEAK
    decl.type_of = produce_function_type(state, f.data, decl.name);
    decl.data.scope_handle = apply_types_to_owned_block(state, f.data.block_handle);
    return decl;
}

Typed<VariableDecl> var_decl(Typing *state, UntypedDecl<UntypedVar> untyped) {

    Typed<VariableDecl> typed;
    typed.name = copy_decl_name(untyped.name);
    typed.data.flags = 0;
    typed.data.initial_value.type_of = Error_Type();

    // Inferred e.g:
    // let i = 10
    //
    if (untyped.data.given_type.tag == EXPR_NONE) {

        typed.data.flags |= VARIABLE_IS_INFERRED;
        
        // Declaration has no type-name or value.
        // let i
        if (untyped.data.expr.tag == EXPR_NONE) {
            printf("error: not enough information to infer type of '%.*s'. Please provide either a type-name, or an initial value.\n",
                typed.name.length, typed.name.data);

            assert(false);
        }

        typed.data.flags |= VARIABLE_IS_INITED;

        typed.type_of = compute_type_of_expression(state, untyped.data.expr);
        typed.data.initial_value.type_of = typed.type_of;

        return typed;
    }
    
    // Explicit e.g:
    // let i int = 10
    //

    if (untyped.data.given_type.tag == EXPR_DIRECTIVE) {
        // Handles compile-time type-names such as '#int'

        auto inner_expr = *untyped.data.given_type.directive;
        
        // const i #int = 10
        //          ^^^^^^^^ binary expression
        //
        // The left hand side is the type, and the right-hand-side is the initial value expression.
        if (inner_expr.tag == EXPR_BINARY) {
            
            // Type-name expression
            auto lhs = inner_expr.binary.left;

            // Flag it as compile-time type.
            typed.type_of = resolve_expression_as_typename(state, *lhs);
            typed.type_of.flags |= TYPE_IS_COMPILE_TIME;
            
            // Compute the type of the value expression and store it.
            typed.data.initial_value.type_of = compute_type_of_expression(state, *inner_expr.binary.right);
            typed.data.flags |= VARIABLE_IS_INITED;

            return typed;
        }

        TypeHandle inner = resolve_expression_as_typename(state, inner_expr);
        inner.flags |= TYPE_IS_COMPILE_TIME;
        typed.type_of = inner;
    }

    else { // type-name is not a directive
        typed.type_of = resolve_expression_as_typename(state, untyped.data.given_type);
    }

    if (untyped.data.expr.tag != EXPR_NONE) {
        // Declaration was given an initial value
        typed.data.flags |= VARIABLE_IS_INITED;
        typed.data.initial_value.type_of = compute_type_of_expression(state, untyped.data.expr);
    }

    return typed;
}

void init_types(TypedFile *of) {
    assert(of->symbol_table.backing.data);

    array_init(&of->all_types, 24); // LEAK

    auto int_name = copy_string("int");
    table_append(&of->symbol_table, int_name, {
        .tag = DECL_TYPE,
        .slot = array_append(&of->all_types, {
            .tag = TYPE_PRIMITIVE_INT,
            .name = int_name,
            .size_in_bytes = 4,
            .metadata = nullptr,
        }),
    });

    auto string_name = copy_string("string");
    table_append(&of->symbol_table, string_name, {
        .tag = DECL_TYPE,
        .slot = array_append(&of->all_types, {
            .tag = TYPE_PRIMITIVE_STRING,
            .name = string_name,
            .size_in_bytes = 4,
            .metadata = nullptr,
        }),
    });
}

int apply_types_and_build_symbol_tables(Array<UntypedFile> to, Array<TypedFile> *output) {

    for (int i = 0; i < to.length; i++) {
        auto code = to[i];
        auto top_level = code.ast[0];

        Typing state = { .from_ast = code.ast };
        table_init(&state.into.symbol_table); // LEAK
        array_init(&state.into.function_decls, top_level.func_decls.length); // LEAK
        array_init(&state.into.variable_decls, top_level.var_decls.length); // LEAK
        array_init(&state.into.all_scopes, code.ast.length); // LEAK
        init_types(&state.into); // LEAK

        for (int j = 0; j < top_level.func_decls.length; j++) {

            auto f = function_decl(&state, top_level.func_decls[j]);

            int slot = array_append(&state.into.function_decls, f);
            table_append(&state.into.symbol_table, f.name, {
                .tag = DECL_FUNCTION,
                .slot = slot,
            });
        }

        for (int j = 0; j < top_level.var_decls.length; j++) {

            auto v = var_decl(&state, top_level.var_decls[j]);
            int slot = array_append(&state.into.variable_decls, v);

            table_append(&state.into.symbol_table, v.name, {
                .tag = DECL_VARIABLE,
                .slot = slot,
            });
        }

        array_append(output, state.into);
    }

    return 0;
}
