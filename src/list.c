// list

PRIM(cons)
{
  if (argc != 2) return object_argument_error;
  return new_cons(eval(arg(0)), eval(arg(1)));
}

PRIM(car)
{
  if ((argc != 1 && argc != 2) || !object_listp(arg(0)))
    return object_argument_error;
  if (argc == 1) return object_car(arg(0));
  *(arg(0)->cons.car) = *arg(1);
  return arg(0);
}

PRIM(cdr)
{
  if ((argc != 1 && argc != 2) || !object_listp(arg(0)))
    return object_argument_error;
  if (argc == 1) return object_cdr(arg(0));
  *(arg(0)->cons.cdr) = *arg(1);
  return arg(0);
}
