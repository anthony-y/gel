#pragma once

enum ResultTag {
    Ok,
    Error,
};
template<typename T> struct Result {
    union {
        T ok;
        bool error;
    };
    ResultTag tag;

    T unwrap();
};
template<typename T> T Result<T>::unwrap() {
    assert(this->tag == Ok);
    return ok;
}

#define Err_Result(T) (Result<T>{.error=true, .tag=Error})
