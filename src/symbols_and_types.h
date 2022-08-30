#pragma once

#include "common.h"
#include "arrays.h"
#include "result.h"
#include "table.h"
#include "ast.h"

#define GEL_GENERIC_QUEUED_TYPE_SLOT 0

using TypedFileHandle = int;
using TypedExprHandle = int;
using ScopeHandle = int;

struct TypeHandle;

enum TypedDeclTag {
    DECL_FUNCTION,
    DECL_VARIABLE,
    DECL_VARIANT,
    DECL_STRUCT,

    // Used to represent primitive types in the symbol table.
    //
    // However, this can also be used to access directly the type of e.g. SYMBOL_VARIABLE
    // without polluting the cache with the entire declaration.
    DECL_TYPE,

    DECL_QUEUED_VAR,
};

struct TypedDeclHandle {
    TypedDeclTag tag; // which array should we look in?
    int slot; // where in the array?
};

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

    TYPE_QUEUED,
};

enum TypeFlags : int {
    TYPE_IS_SIGNED = 0x1 << 0, // for integer types, is it signed?
    TYPE_IS_POLYMORPHIC = 0x1 << 2,
    TYPE_IS_ALWAYS_COMPILE_TIME = 0x1 << 3, // not used yet but I anticipate it's use
};

enum TypeHandleFlags : int {
    TYPE_HANDLE_IS_COMPILE_TIME = 0x1 << 0,
    TYPE_HANDLE_IS_POINTER = 0x1 << 1,
    TYPE_HANDLE_IS_POLYMORPHIC = 0x1 << 2,
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
    Buffer name;
    int size_in_bytes;
    int flags;
    TypeMetadata *metadata;
};

struct TypeHandle {
    int flags = 0;
    int slot = -1;

    bool operator == (const TypeHandle &right) {
        return right.flags == flags && right.slot == slot;
    }

    bool operator != (const TypeHandle &right) {
        return right.flags != flags || right.slot != slot;
    }
};

enum TypedExprTag {
    TEXPR_QUEUED,

    TEXPR_TYPE_NAME,
    TEXPR_NONE,
    TEXPR_BINARY,
    TEXPR_UNARY,
    TEXPR_IDENTIFIER,
    TEXPR_INT_LITERAL,
    TEXPR_STRING_LITERAL,
    TEXPR_FLOAT_LITERAL,
    TEXPR_FUNCTION_CALL,
    TEXPR_ARRAY_VIEW,
    TEXPR_VIEW_TYPE,
    TEXPR_ARRAY_TYPE,
    TEXPR_DIRECTIVE,
    TEXPR_IF,
    TEXPR_MATCH,
    TEXPR_MATCHER,
    TEXPR_PARENS,
    TEXPR_USING,
};

struct TypedExpr {
    TypeHandle type_of;
    union {
        int int_literal;
        Buffer string_literal;
    };
};

// Encodes a thing which either produces, or is itself of, a specified type.
template<typename T> struct Typed {

    // type_of:
    //   for inferred variable decls: the type of the value
    //   for explicit variable decls: the given type
    //   for function decls: the type of the signature
    //   for variant & struct decls: the resulting type of the declaration
    //
    TypeHandle type_of;

    Buffer name;

    T data;
};

struct StructDecl {
    ScopeHandle scope_handle;
};

struct VariantDecl {
    ScopeHandle scope_handle;
};

struct FunctionDecl {
    ScopeHandle scope_handle;
    TypeHandle return_type;
};

enum VariableFlags {
    VARIABLE_IS_INFERRED = 0x1 << 0, // variable was not explicitly given a type at it's declaration.
    VARIABLE_IS_CONST = 0x1 << 1, // variable was marked as const at it's declaration.
    VARIABLE_IS_INITED = 0x1 << 2, // variable was given an initial value at it's declaration.
};

struct VariableDecl {
    int flags = 0;
    TypedExpr initial_value;
};

struct Scope {
    ScopeHandle slot;
    Array<TypedExpr> statements;
    Array<Typed<VariableDecl>> locals;
};

struct TypedFile {

    // Enables random access to the arrays by mapping each element to a `TypedDeclHandle`.
    Table<TypedDeclHandle> symbol_table;


    // `TypedDeclHandle` describes which one of these arrays contains the data, and where.
    //
    Array<Typed<StructDecl>> struct_decls;     // DECL_STRUCT
    Array<Typed<VariantDecl>> variant_decls;   // DECL_VARIANT
    Array<Typed<FunctionDecl>> function_decls; // DECL_FUNCTION
    Array<Typed<VariableDecl>> variable_decls; // DECL_VARIABLE
    Array<Type> all_types;                     // DECL_TYPE (including queued types)
    
    Array<UntypedDecl<UntypedVar>> queue;      // DECL_QUEUED_VAR


    // These two are not mapped by the `symbol_table`
    //
    Array<Scope> all_scopes; // indexed by ScopeHandle
    BucketArray<TypedExpr> nested_expressions; // allocator for sub-expressions
};

int apply_types_and_build_symbol_tables(Array<UntypedFile> to, Array<TypedFile> *output);
