// lambda/macro

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

DEFUN(special_operator_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, SPECIAL));
  return TRUE;
}

DEFUN(builtin_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case FUNCITON:
    case SPECIAL:
      *result = object_true;
      return TRUE;
    default:
      *result = object_nil;
      return TRUE;
  }
}

DEFUN(builtin_name)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case FUNCITON:
    case SPECIAL:
      *result = argv->cons.car->builtin.name;
      return TRUE;
    default:
      return FALSE;
  }
}

DEFUN(function_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case FUNCITON:
    case LAMBDA:
      *result = object_bool(TRUE);
      break;
    default:
      *result = object_bool(FALSE);
      break;
  }
  return TRUE;
}

DEFUN(macro_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, MACRO));
  return TRUE;
}

DEFUN(lambda_parameter)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_lambda(argv->cons.car, &o)) return FALSE;
  *result = o->lambda.params;
  return TRUE;
}

DEFUN(lambda_body)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_lambda(argv->cons.car, &o)) return FALSE;
  *result = o->lambda.body;
  return TRUE;
}
