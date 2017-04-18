/*
  paren primitive.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "prim.h"

struct Object *Object_new(int type, void *val) {
  struct Object *obj;
  if ((obj = (struct Object *)calloc(1, sizeof(struct Object))) == NULL) {
    fprintf(stderr, "Object_new: Cannot allocate memory.");
    exit(1);
  }
  obj->type = type;
  if (type == STRING)
    obj->val.string = (char *)val;
  else if (type == KEYWORD)
    obj->val.keyword = (char *)val;
  else if (type == CHARACTER)
    obj->val.character = ((char *)val)[0];
  else if (type == SYMBOL)
    obj->val.symbol = (char *)val;
  else if (type == INTEGER)
    obj->val.integer = *(int *)val;
  else if (type == DOUBLE)
    obj->val.dfloat = *(double *)val;
  else  if (type == FUNCTION)
    obj->val.function = (struct Ast *)val;
  else {
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

char *asString(struct Object *obj) {
  static char str[100];
  if (obj->type == STRING)
    return obj->val.string;
  else if (obj->type == KEYWORD)
    return obj->val.keyword;
  else if (obj->type == CHARACTER) {
    str[0] = obj->val.character;
    str[1] = '\0';
    return str;
  }
  else if (obj->type == SYMBOL)
    return obj->val.symbol;
  else if (obj->type == INTEGER) {
    sprintf(str, "%d", obj->val.integer);
    return str;
  }
  else if (obj->type == DOUBLE) {
    sprintf(str, "%f", obj->val.dfloat);
    return str;
  }
  else
    return "asString: unknown type.";
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
