/*
  paren primitive.
*/

#include "prim.h"

static int i;

void Prim_init(struct Env *env) {
  i = 100;
  Env_install(env, "i", SYMBOL, &i);
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
