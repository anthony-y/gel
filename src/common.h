#pragma once

using usize = unsigned long int;
using isize = signed long int;

using u32 = unsigned int;
using s32 = int;

using u64 = unsigned long int;
using s64 = signed long int;

using s8 = signed char;
using u8 = char;

using f32 = float;
using f64 = double;

struct Buffer {
   u8    *data;
   usize  length;
};
