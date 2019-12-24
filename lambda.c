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
  *result = object_bool(typep(argv->cons.car, SPECIAL));
  return TRUE;
}

DEFUN(builtin_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, FUNCITON));
  return TRUE;
}

DEFUN(builtin_name)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (typep(argv->cons.car, FUNCITON) || typep(argv->cons.car, FUNCITON)) {
    *result = argv->cons.car->builtin.name;
    return TRUE;
  }
  return FALSE;
}

DEFUN(lambda_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, LAMBDA)
      || typep(argv->cons.car, FUNCITON));
  return TRUE;
}

DEFUN(macro_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, MACRO));
  return TRUE;
}

DEFUN(lambda_parameter)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!typep(o = argv->cons.car, LAMBDA) && !typep(o, MACRO)) return FALSE;
  *result = o->lambda.params;
  return TRUE;
}

DEFUN(lambda_body)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!typep(o = argv->cons.car, LAMBDA) && !typep(o, MACRO)) return FALSE;
  *result = o->lambda.body;
  return TRUE;
}
