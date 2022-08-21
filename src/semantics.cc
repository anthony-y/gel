#include "semantics.h"
#include "common.h"
#include "table.h"
#include "arrays.h"
#include "symbols_and_types.h"

#include <stdio.h>

int ensure_code_is_semantically_correct(Array<TypedFile> code) {

    for (int i = 0; i < code[0].variable_decls.length; i++) {
        auto v = code[0].variable_decls[i];
        
        // If there is an initial value given to the declaration
        if (v.data.flags & VARIABLE_IS_INITED) {

            // If the type of the expression doesn't match the one given type. 
            if (v.data.initial_value.type_of != v.type_of) {
                printf("error: type mismatch: '%.*s'\n", v.name.length, v.name.data);
                return -1;
            }

        }
    }

    return 0;
}
