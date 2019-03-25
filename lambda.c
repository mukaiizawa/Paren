// lambda/macro

#include "std.h"
#include "xsplay.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

PRIM(lambda_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, LAMBDA));
  return TRUE;
}

PRIM(macro_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, MACRO));
  return TRUE;
}

PRIM(lambda_parameter)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!typep(o = argv->cons.car, LAMBDA)) return FALSE;
  *result = o->lambda.params;
  return TRUE;
}

PRIM(lambda_body)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!typep(o = argv->cons.car, LAMBDA)) return FALSE;
  *result = o->lambda.body;
  return TRUE;
}
