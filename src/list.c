// list

PRIM(cons)
{
  if (argc != 2) return FALSE;
  *result = new_cons(eval(arg(0)), eval(arg(1)));
  return TRUE;
}

PRIM(car)
{
  if ((argc != 1 && argc != 2) || !object_listp(arg(0))) return FALSE;
  if (argc == 1) *result = object_car(arg(0));
  else {
    *(arg(0)->cons.car) = *arg(1);
    *result = arg(0);
  }
  return TRUE;
}

PRIM(cdr)
{
  if ((argc != 1 && argc != 2) || !object_listp(arg(0))) return FALSE;
  if (argc == 1) *result = object_cdr(arg(0));
  else {
    *(arg(0)->cons.cdr) = *arg(1);
    *result = arg(0);
  }
  return TRUE;
}
