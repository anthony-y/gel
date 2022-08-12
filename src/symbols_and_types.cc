#include "symbols_and_types.h"
#include "common.h"
#include "arrays.h"
#include "table.h"
#include "ast.h"
#include "defer.h"

TypedDeclHandle function_decl(UntypedFunc f) {
}

TypedDeclHandle var_decl(UntypedVar v) {
}

int apply_types_and_build_symbol_tables(Array<UntypedFile> to, Array<TypedFile> *output) {

    for (int i = 0; i < to.length; i++) {
        auto code = to[i];
        auto top_level = code[0];

        for (int j = 0; j < top_level.func_decls.length; j++) {
            function_decl(top_level.func_decls[j], output); 
        }

        for (int j = 0; j < top_level.var_decls.length; j++) {
            var_decl(top_level.var_decls[j], output); 
        }
    }

    return 0;
}
