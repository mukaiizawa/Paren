// primitive

#define arg(n) object_nth(args, n)
#define argc object_length(args)

#define PRIM(name) int prim_##name(object self,object *args,object *result)

// #define PRIM(name) int prim_##name(object args, object *result)
// #define arg(n) object_nth(args, n)
// #define argc object_length(args)
// #include "prim.wk"
// #undef argc
// #undef arg
// #undef PRIM
