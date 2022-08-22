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
    ScopeHandle current_scope = 0;
};

static void push_scope(Typing *state) {
    Scope scope;
    scope.slot = state->current_scope++;
    array_init(&scope.locals, 10);
    array_append(&state->into.all_scopes, scope);
}

static void pop_scope(Typing *state) {
    state->current_scope--;
}

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

static TypeHandle resolve_expression_as_typename(Typing *state, UntypedExpr type_name) {

    switch (type_name.tag) {

    case EXPR_NONE: return Error_Type();

    case EXPR_BINARY: return Error_Type();

    case EXPR_FLOAT_LITERAL: return Error_Type();

    case EXPR_ARRAY_TYPE: return Error_Type();

    // e.g.
    // #import("file")
    //
    case EXPR_DIRECTIVE: {
        auto inner_handle = resolve_expression_as_typename(state, *type_name.directive);
        TypeHandle wrapped;
        wrapped.flags = inner_handle.flags | TYPE_IS_COMPILE_TIME;
        wrapped.slot = inner_handle.slot;
        return wrapped;
    } break;

    // e.g.
    // let f Foo
    //       ^^^
    case EXPR_IDENTIFIER: {
        
        Buffer identifier = type_name.identifier;
        Result<TypedDeclHandle> result = table_get(state->into.symbol_table, identifier);

        if (result.tag == Error) {

            // Add a new type to the array, with placeholder data
            // Add its handle to the symbol table
            // Later, when we come across its declaration, we can just overwrite the type data.
            // Just before we do semantic checking, we can iterate all the types and error if there are any placeholders left.


            // We'll insert a placeholder type with it's name.
            Type queued = {
                .tag = TYPE_QUEUED,
                .name = identifier,
                .size_in_bytes = 0,
                .metadata = nullptr,
            };

            // We create a handle to access it via the type array.
            TypeHandle queued_handle;
            queued_handle.slot = array_append(&state->into.all_types, queued);

            // We also create a handle to access it via the symbol table.
            table_append(&state->into.symbol_table, identifier, {
                .tag  = DECL_TYPE,
                .slot = queued_handle.slot,
            });

            return queued_handle;
        }

        TypedDeclHandle symbol_handle = result.ok;

        switch (symbol_handle.tag) {

        case DECL_TYPE: {
            TypeHandle as_type_handle = Symbol_As_Type(symbol_handle.slot);
            if (state->into.all_types[as_type_handle.slot].tag == TYPE_QUEUED) {
                // return as_type_handle;
            }
            return as_type_handle;
        } break;
            
        case DECL_VARIABLE: {
            auto v = state->into.variable_decls[symbol_handle.slot];

            printf("variable '%.*s' is being used as a type-name, but it's declaration does not produce a type\n.",
                v.name.length, v.name.data);
            
            return Error_Type();
        } break;

        case DECL_FUNCTION: assert(false);
        
        case DECL_STRUCT: return state->into.struct_decls[symbol_handle.slot].type_of;

        case DECL_VARIANT: return state->into.variant_decls[symbol_handle.slot].type_of;

        default: assert(false);

        }

    } break;

    }

    return Error_Type();
}

static TypeHandle produce_function_type(Typing *state, UntypedFunc from, Buffer copied_name) {
    return Error_Type();
}

static TypeHandle produce_variant_type(Typing *state, UntypedVariant from, Buffer copied_name) {
    return Error_Type();
}

static TypeHandle produce_struct_type(Typing *state, UntypedStruct from, Buffer copied_name) {
    return Error_Type();
}

/*
This should return a TypedExpr and be renamed to `apply_type_to_expression` or something.

Also, literals should probably have their values deciphered here
    (and we just store a Buffer in the UntypedExpr)

*/
static TypeHandle compute_type_of_expression(Typing *state, UntypedExpr of) {

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

static ScopeHandle apply_types_to_owned_block(Typing *state, UntypedBlockHandle block_handle) {
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

static Typed<FunctionDecl> function_decl(Typing *state, UntypedDecl<UntypedFunc> f) {
    Typed<FunctionDecl> decl;
    decl.name = copy_decl_name(f.name); // LEAK
    decl.type_of = produce_function_type(state, f.data, decl.name);
    decl.data.scope_handle = apply_types_to_owned_block(state, f.data.block_handle);
    return decl;
}

static Typed<StructDecl> struct_decl(Typing *state, UntypedDecl<UntypedStruct> s) {
    Typed<StructDecl> decl;
    decl.name = copy_decl_name(s.name);
    decl.type_of = produce_struct_type(state, s.data, decl.name);
    decl.data.scope_handle = apply_types_to_owned_block(state, s.data.block_handle);
    return decl;
}

static Typed<VariantDecl> variant_decl(Typing *state, UntypedDecl<UntypedVariant> s) {
    Typed<VariantDecl> decl;
    decl.name = copy_decl_name(s.name);
    decl.type_of = produce_variant_type(state, s.data, decl.name);
    decl.data.scope_handle = apply_types_to_owned_block(state, s.data.block_handle);
    return decl;
}

static Typed<VariableDecl> var_decl(Typing *state, UntypedDecl<UntypedVar> untyped) {

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

    else if (untyped.data.given_type.tag == EXPR_DIRECTIVE) {
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

    else { // explicit type-name is present, not a directive
        typed.type_of = resolve_expression_as_typename(state, untyped.data.given_type);
    }

    if (untyped.data.expr.tag != EXPR_NONE) {
        // Declaration was given an initial value
        typed.data.flags |= VARIABLE_IS_INITED;
        typed.data.initial_value.type_of = compute_type_of_expression(state, untyped.data.expr);
    }

    return typed;
}

static void init_types(TypedFile *of) {
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

    int error_count = 0; // TODO actual error function

    for (int i = 0; i < to.length; i++) {
        auto code = to[i];
        auto top_level = code.ast[0];

        Typing state = { .from_ast = code.ast };
        table_init(&state.into.symbol_table); // LEAK
        array_init(&state.into.function_decls, top_level.func_decls.length); // LEAK
        array_init(&state.into.variable_decls, top_level.var_decls.length); // LEAK
        array_init(&state.into.struct_decls, top_level.struct_decls.length); // LEAK
        array_init(&state.into.variant_decls, top_level.variant_decls.length); // LEAK
        array_init(&state.into.all_scopes, code.ast.length); // LEAK
        init_types(&state.into); // LEAK

        for (int j = 0; j < top_level.struct_decls.length; j++) {

            auto s = struct_decl(&state, top_level.struct_decls[j]);
            int slot = array_append(&state.into.struct_decls, s);

            // The struct may already be in the symbol table as a placeholder.
            auto maybe_used = table_get(state.into.symbol_table, s.name);

            // If it is, we replace the old placeholder value with the real declaration.
            if (maybe_used.tag == Ok) {

                TypedDeclHandle existing = maybe_used.ok;
                if (existing.tag != DECL_TYPE) {
                    printf("redefinition of type '%.*s'.\n", s.name.length, s.name.data);
                    error_count++;
                }

                table_replace(&state.into.symbol_table, s.name, {
                    .tag = DECL_STRUCT,
                    .slot = slot,
                });
            }

            else { // otherwise we add a new entry for it.
                table_append(&state.into.symbol_table, s.name, {
                    .tag = DECL_STRUCT,
                    .slot = slot,
                });
            }
        }

        for (int j = 0; j < top_level.variant_decls.length; j++) {

            auto s = variant_decl(&state, top_level.variant_decls[j]);
            int slot = array_append(&state.into.variant_decls, s);

            // The struct may already be in the symbol table as a placeholder.
            auto maybe_used = table_get(state.into.symbol_table, s.name);

            // If it is, we replace the old placeholder value with the real declaration.
            if (maybe_used.tag == Ok) {
                table_replace(&state.into.symbol_table, s.name, {
                    .tag = DECL_VARIANT,
                    .slot = slot,
                });
            }

            else { // otherwise we add a new entry for it.
                table_append(&state.into.symbol_table, s.name, {
                    .tag = DECL_VARIANT,
                    .slot = slot,
                });
            }
        }

        for (int j = 0; j < top_level.var_decls.length; j++) {

            auto v = var_decl(&state, top_level.var_decls[j]);
            int slot = array_append(&state.into.variable_decls, v);

            table_append(&state.into.symbol_table, v.name, {
                .tag = DECL_VARIABLE,
                .slot = slot,
            });
        }

        for (int j = 0; j < top_level.func_decls.length; j++) {

            auto f = function_decl(&state, top_level.func_decls[j]);

            int slot = array_append(&state.into.function_decls, f);
            table_append(&state.into.symbol_table, f.name, {
                .tag = DECL_FUNCTION,
                .slot = slot,
            });
        }

        array_append(output, state.into);
    }

    return error_count;
}
