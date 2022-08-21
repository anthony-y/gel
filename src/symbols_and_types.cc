#include "symbols_and_types.h"
#include "common.h"
#include "arrays.h"
#include "table.h"
#include "ast.h"
#include "defer.h"

// (temp) printf
#include <stdio.h>

#include <string_view>

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

    case EXPR_NONE: return -1;

    case EXPR_BINARY: return -1;

    case EXPR_FLOAT_LITERAL: return -1;

    case EXPR_ARRAY_TYPE: return -1;

    case EXPR_IDENTIFIER: {
        
        Buffer identifier = type_name.identifier;
        Result<TypedDeclHandle> decl_handle = table_get(state->into.symbol_table, identifier);

        if (decl_handle.tag == Error) {
            printf("no such type '%.*s'\n", identifier.length, identifier.data);
            return -1;
        }

        return decl_handle.ok.slot;

    } break;

    }

    return -1;
}

TypeHandle produce_function_type(Typing *state, UntypedFunc f, Buffer copied_name) {
    return -1;
}

TypeHandle produce_variant_type(Typing *state, UntypedVariant v, Buffer copied_name) {
    return -1;
}

TypeHandle produce_struct_type(Typing *state, UntypedStruct s, Buffer copied_name) {
    return -1;
}

TypeHandle compute_type_of_expression(Typing *state, UntypedExpr of) {

    switch (of.tag) {

    case EXPR_INT_LITERAL: {   
        auto result = table_get(state->into.symbol_table, copy_string("int"));
        assert(result.tag == Ok);
        assert(result.ok.tag == DECL_TYPE);
        return result.ok.slot;
    } break;

    case EXPR_STRING_LITERAL: {
        auto result = table_get(state->into.symbol_table, copy_string("string"));
        assert(result.tag == Ok);
        assert(result.ok.tag == DECL_TYPE);
        return result.ok.slot;
    } break;

    }

    return -1;
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

Typed<VariableDecl> var_decl(Typing *state, UntypedDecl<UntypedVar> v) {

    Typed<VariableDecl> decl;
    decl.name = copy_decl_name(v.name);
    decl.data.flags = 0;
    decl.data.initial_value.type_of = -1;

    // Inferred e.g:
    // let i = 10
    //
    if (v.data.given_type.tag == EXPR_NONE) {

        decl.data.flags |= VARIABLE_IS_INFERRED;
        
        // Declaration has no type-name or value.
        // let i
        if (v.data.expr.tag == EXPR_NONE) {
            printf("error: not enough information to infer type of '%.*s'. Please provide either a type-name, or an initial value.\n",
                decl.name.length, decl.name.data);

            assert(false);
        }

        decl.data.flags |= VARIABLE_IS_INITED;

        decl.type_of = compute_type_of_expression(state, v.data.expr);
        decl.data.initial_value.type_of = decl.type_of;

        return decl;
    }
    
    // Explicit e.g:
    // let i int = 10
    //
    decl.type_of = resolve_expression_as_typename(state, v.data.given_type);

    if (v.data.expr.tag != EXPR_NONE) {
        decl.data.flags |= VARIABLE_IS_INITED;
        decl.data.initial_value.type_of = compute_type_of_expression(state, v.data.expr);
    }

    return decl;
}

void init_types(TypedFile *of) {
    assert(of->symbol_table.backing.data);

    array_init(&of->all_types, 24); // LEAK

    auto int_name = copy_string("int");
    table_append(&of->symbol_table, int_name, {
        .tag = DECL_TYPE,
        .slot = array_append(&of->all_types, {
            .tag = TYPE_PRIMITIVE_INT,
            .flags = 0,
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
            .flags = 0,
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
