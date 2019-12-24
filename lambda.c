// lambda/macro

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

DEFUN(special_operator_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, SPECIAL));
  return TRUE;
}

DEFUN(builtin_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(builtin_p(argv->cons.car));
  return TRUE;
}

DEFUN(builtin_name)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (builtin_p(argv->cons.car)) {
    *result = argv->cons.car->builtin.name;
    return TRUE;
  }
  return FALSE;
}

DEFUN(function_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  switch (type(argv->cons.car)) {
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
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, MACRO));
  return TRUE;
}

DEFUN(lambda_parameter)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!type_p(o = argv->cons.car, LAMBDA) && !type_p(o, MACRO)) return FALSE;
  *result = o->lambda.params;
  return TRUE;
}

DEFUN(lambda_body)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!type_p(o = argv->cons.car, LAMBDA) && !type_p(o, MACRO)) return FALSE;
  *result = o->lambda.body;
  return TRUE;
}
