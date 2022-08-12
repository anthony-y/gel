#include "symbols_and_types.h"
#include "common.h"
#include "arrays.h"
#include "table.h"
#include "ast.h"
#include "defer.h"

void function_decl(UntypedDecl<UntypedFunc> f) {
}

void var_decl(UntypedDecl<UntypedVar> v) {
}

int apply_types_and_build_symbol_tables(Array<UntypedFile> to, Array<TypedFile> *output) {

    for (int i = 0; i < to.length; i++) {
        auto code = to[i];
        auto top_level = code.ast[0];

        for (int j = 0; j < top_level.func_decls.length; j++) {
            function_decl(top_level.func_decls[j]); 
        }

        for (int j = 0; j < top_level.var_decls.length; j++) {
            var_decl(top_level.var_decls[j]); 
        }
    }

    return 0;
}
