#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "lexing.h"
#include "ast.h"
#include "arrays.h"
#include "defer.h"
#include "result.h"
#include "symbols_and_types.h"
#include "semantics.h"
#include "table.h"

#define DEBUG_MODE 0

static Result<Buffer> read_entire_file(const u8 *path);

template<typename T> static void copy_to_flat_array_and_free_buckets(BucketArray<T> array, Array<T> *out_flat) {
    
    array_init(out_flat, array.length * BucketArray<T>::BUCKET_MAX_ITEMS);

    auto next = array.first_bucket;
    while (next) {
        auto current = next;
        for (u64 i = 0; i < current->length; i++) {
            T data = current->items[i];
            out_flat->data[out_flat->length++] = data;
        }
        next = current->next_bucket;
        free(current);
    }
}

Buffer copy_token_string(TokenData t) {
    u8 *data = (u8 *)malloc(t.length+1);
    strncpy(data, t.start, t.length);
    return { .data = data, .length = (usize)t.length };
}

Buffer copy_string(const u8 *t) {
    const usize length = strlen(t);
    u8 *data = (u8 *)malloc(length+1);
    strncpy(data, t, length);
    return { .data = data, .length = length };
}

int do_frontend(BucketArray<UntypedFile> *all_untyped_files, u8 *file_path) {

    auto maybe_file_data = read_entire_file(file_path);
    if (maybe_file_data.tag == Error) {
        return -1;
    }
    auto file_data = maybe_file_data.ok;

    BucketArray<TokenData> tokens;
    Array<TokenData> flat;
    bucket_array_init(&tokens);

    if (!do_lexical_analysis(file_data, &tokens)) {
        return -1;
    }

    copy_to_flat_array_and_free_buckets(tokens, &flat);

    Array<UntypedCode> ast;
    int success = do_parsing(flat, &ast);
    if (success < 0)
        return -1;

    UntypedCode top_level = ast[0];

    for (int i = 0; i < top_level.top_directives.length; i++) {
        UntypedExpr d = top_level.top_directives[i];
        if (d.tag == EXPR_FUNCTION_CALL) {

            auto callname = d.function_call.name;
            assert(callname->tag == EXPR_IDENTIFIER);
            auto buffer = callname->identifier;
            if (strncmp(buffer.data, "import", buffer.length) == 0) {

                auto name = d.function_call.given_arguments[0];
                assert(name.tag == EXPR_STRING_LITERAL);

                // TODO: memory leak
                auto path = (u8 *)malloc(name.string_literal.length);
                strncpy(path, name.string_literal.data+1, name.string_literal.length-2);
                path[name.string_literal.length-2] = 0;

                do_frontend(all_untyped_files, path);
            }
        }
    }

    int slot = all_untyped_files->length+1;
    bucket_array_append(all_untyped_files, UntypedFile {
            .slot = slot,
            .token_data = flat,
            .ast = ast,
            .file_data = file_data,
    });
    return slot;
}

static void deinit_untyped_files(Array<UntypedFile> *files) {
    for (int i = 0; i < files->length; i++) {
        auto ast = files->data[i].ast;
        for (int j = 0; j < ast.length; j++) {
            auto a = &ast[j];
            bucket_array_free(&a->nested_expressions);
            array_free(&a->var_decls);
            array_free(&a->func_decls);
            array_free(&a->all_statements);
            array_free(&a->top_directives);
        }
        array_free(&files->data[i].token_data);
        free(files->data[i].file_data.data);
    }
}

static bool test_table() {
    Table<int> t;
    table_init(&t);
    Defer (table_free(&t));
    
    table_append(&t, copy_string("One"), 1);
    Result<int> maybe_int = table_get(t, copy_string("One"));
    bool a = maybe_int.tag == Ok && maybe_int.ok == 1;

    table_append(&t, copy_string("One-hundred and twenty three"), 123);
    Result<int> maybe_123 = table_get(t, copy_string("One-hundred and twenty three"));
    bool b = maybe_123.tag == Ok && maybe_123.ok == 123;

    table_append(&t, copy_string("My string"), 314159);
    Result<int> hopefully_none = table_get(t, copy_string("My incorrect string"));
    bool c = hopefully_none.tag == Error && hopefully_none.error == true;

    return (a && b && c);
}

int main(int args_count, char *args[]) {

    if (args_count < 2) {
        printf("gel: please supply the main compilation target (e.g. src/main.gel)");
        return 1;
    }

    assert(test_table());

    auto main_file_path = (u8 *)args[1];

    BucketArray<UntypedFile> all_untyped_files;
    bucket_array_init(&all_untyped_files);

    int main_file_slot = do_frontend(&all_untyped_files, main_file_path);
    if (main_file_slot < 0) {
        return 1;
    }

    Array<UntypedFile> flat_file_array;
    copy_to_flat_array_and_free_buckets(all_untyped_files, &flat_file_array);
    Defer ({
        deinit_untyped_files(&flat_file_array);
        array_free(&flat_file_array);
    });

    Array<TypedFile> typed_files;
    int typing_status = apply_types_and_build_symbol_tables(flat_file_array, &typed_files);
    if (typing_status < 0) {
        return 1;
    }

    int semantics_status = ensure_code_is_semantically_correct(typed_files);
    if (semantics_status < 0) {
        return 1;
    }

#if 0
    Array<Bytecode> bytecode_modules = compile_to_bytecode(typed_files);
#endif

    printf("gel: Compilation was a success!\n");
    return 0;
}

static Result<Buffer> read_entire_file(const u8 *path) {
    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "gel: error: can't open file '%s'\n", path);
        return Err_Result(Buffer);
    }

    fseek(f, 0, SEEK_END);
    usize file_length = ftell(f);
    rewind(f);

    auto data = (u8 *)malloc(file_length+1);
    if (!data) {
        fprintf(stderr, "gel: error: out of memory: can't allocate file '%s'\n", path);
        return Err_Result(Buffer);
    }
    
    usize buffer_length = fread(data, sizeof(u8), file_length, f);
    if (buffer_length < file_length) {
        fprintf(stderr, "gel: error: can't read file '%s'\n", path);
        return Err_Result(Buffer);
    }

    fclose(f);

    data[buffer_length] = 0;

    auto b = Buffer{.data=data, .length=buffer_length};
    return Result<Buffer>{.ok=b, .tag=Ok};
}
