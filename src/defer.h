#pragma once

// Defer, adapted (I changed some identifiers lol) from this article by Ignacio Castaño:
// http://the-witness.net/news/2012/11/scopeexit-in-c11/
//
#define String_Join2(arg1, arg2) Do_String_Join2(arg1, arg2)
#define Do_String_Join2(arg1, arg2) arg1 ## arg2

template <typename F>
struct Deferer
{
    Deferer(F f) : f(f) {}
    ~Deferer() { f(); }
    F f;
};

template <typename F>
Deferer<F> MakeDeferer(F f)
{
    return Deferer<F>(f);
};

#define Defer(code) \
    auto String_Join2(scope_exit_, __LINE__) = MakeDeferer([&](){code;});

