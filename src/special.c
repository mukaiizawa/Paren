// special form

PRIM(assign)
{
  int i, pair;
  object o;
  if ((pair = argc % 2) != 0) return object_argument_error;
  for (i = 0; i < pair; i+= 2) {
    if (!object_typep(arg(i), symbol)) return object_argument_error;
    o = eval(arg(i + 1));
    bind(arg(i), o);
  }
  return o;
}

PRIM(quote)
{
  if (argc != 1) return object_argument_error;
  return arg(0);
}

PRIM(if)
{
  if (argc != 2 && argc != 3) return object_argument_error;
  if (object_bool(arg(0))) return eval(arg(1));
  if (argc == 2) return object_false;
  return eval(arg(2));
}
