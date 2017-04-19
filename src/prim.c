/*
  paren primitive.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "prim.h"

// TODO: cannot set val.string because of object.val is union.
struct Object *Object_new(int type, void *val) {
  struct Object *obj;
  if ((obj = (struct Object *)calloc(1, sizeof(struct Object))) == NULL) {
    fprintf(stderr, "Object_new: Cannot allocate memory.");
    exit(1);
  }
  obj->type = type;
  obj->val.string = (char *)val;
  if (type == KEYWORD)
    obj->val.keyword = (char *)val;
  else if (type == CHARACTER)
    obj->val.character = ((char *)val)[0];
  else if (type == SYMBOL)
    obj->val.symbol = (char *)val;
  else if (type == INTEGER)
    obj->val.integer = atoi((char *)val);
  else if (type == DOUBLE)
    obj->val.dfloat = atof((char *)val);
  else  if (type == FUNCTION)
    obj->val.function = (struct Ast *)val;
  else if (type != STRING) {
    fprintf(stderr, "Object_new: Illegal type.");
    exit(1);
  }
  return obj;
}

void Prim_init(struct Env *env) {
  static int i;
  i = 100;
  // Env_install(env, "i", SYMBOL, &i);
}

struct Object *asString(struct Object *obj) {
  obj->type = STRING;
  return obj;
}

// // +
// char *Paren_plus(struct Ast *ast) {
// char *Paren_plus(struct Ast *ast) {
//   if (strcmp(cmd->val, "+") == 0) {
//     int i;
//     for (i = 0; !Ast_isNil(ope); ope = ope->cdr)
//       i = i + atoi((eval(ope->car, env))->val);
//     if ((ast->val = (char *)malloc(sizeof(char) * (int)(i / 10 + 2))) == NULL) {
//       fprintf(stderr, "+: Cannot allocate memory.");
//       exit(1);
//     }
//     free(ast->val);
//     sprintf(ast->val, "%i", i);
//     ast->type = NUMBER;
//     return ast;
//   }
// }
