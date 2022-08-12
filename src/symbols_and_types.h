#pragma once

#include "common.h"
#include "arrays.h"
#include "result.h"
#include "table.h"
#include "ast.h"

using TypeHandle = int;
using TypedDeclHandle = int;
using TypedFileHandle = int;
using ScopeHandle = int;

enum TypeTag {
    TYPE_PRIMITIVE_INT,
    TYPE_PRIMITIVE_FLOAT,
    TYPE_PRIMITIVE_STRING,
    TYPE_STRUCT,
    TYPE_VARIANT,
    TYPE_FUNCTION_SIGNATURE,
    TYPE_ALIAS,
    TYPE_POINTER,
    TYPE_STACK_ARRAY,
    TYPE_HEAP_ARRAY,
    TYPE_ARRAY_VIEW,
};

enum TypeFlags : int {
    TYPE_IS_SIGNED = 1 << 0,
    TYPE_IS_COMPILE_TIME = 1 << 1,
};

union TypeMetadata {
    struct {
        Table<TypeHandle> members;
    } struct_type;

    struct {
        Array<TypeHandle> argument_types;
        Array<TypeHandle> return_types;
    } func_type;
};

struct Type {
    TypeTag tag;
    int flags;
    Buffer name;
    usize size_in_bytes;
    TypeMetadata *metadata;
};

struct TypedExpr {

};

struct Scope {
    ScopeHandle self;
    Array<TypedExpr> things;
};

template<typename T> struct Typed {
    TypeHandle type_of;
    ScopeHandle residing;
    T data;
};

struct TypedFile {
    Table<TypedDeclHandle> symbol_table;
    Array<Type> all_types;
    Array<Scope> all_scopes;
/*
    Array<Typed<StructDecl>> struct_decls;
    Array<Typed<VariantDecl>> variant_decls;
    Array<Typed<FunctionDecl>> function_decls;
    Array<Typed<VariableDecl>> variable_decls;
*/
};

int apply_types_and_build_symbol_tables(Array<UntypedFile> to, Array<TypedFile> *output);
