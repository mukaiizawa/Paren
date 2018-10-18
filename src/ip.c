// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

// register
#define REG_SIZE 4
static object reg[REG_SIZE]; // general-purpose register.

static struct xsplay special_splay;
static struct xsplay prim_splay;

// error
int ip_trap_code;
static char *error_message;

static void mark_error(char *msg)
{
  ip_trap_code = TRAP_ERROR;
  error_message = msg;
}

// frame
#define STACK_GAP 5
#define FRAME_STACK_SIZE 1000

struct frame {
  int type;
#define APPLY_FRAME 0
#define APPLY_PRIM_FRAME 1
#define BIND_FRAME 2
#define BIND_PROPAGATION_FRAME 3
#define EVAL_FRAME 4
#define EVAL_LOCAL_VAR_FRAME 5
#define EVAL_OPERANDS_FRAME 6
#define EVAL_OPERATOR_FRAME 7
#define EVAL_SEQUENTIAL_FRAME 8
#define IF_FRAME 9
  object local_vars[1];
};

static int sp;
struct frame *fs[FRAME_STACK_SIZE];

static int frame_size(int type)
{
  switch (type) {
    case APPLY_FRAME:
    case APPLY_PRIM_FRAME:
    case EVAL_FRAME:
      return 1;
    case BIND_PROPAGATION_FRAME:
    case EVAL_LOCAL_VAR_FRAME:
    case EVAL_OPERATOR_FRAME:
    case EVAL_SEQUENTIAL_FRAME:
    case IF_FRAME:
      return 2;
    case BIND_FRAME:
    case EVAL_OPERANDS_FRAME:
      return 3;
    default: xassert(FALSE);
  }
  return FALSE;
}

static char *frame_name(int type)
{
  switch (type) {
    case APPLY_FRAME: return "APPLY_FRAME";
    case APPLY_PRIM_FRAME: return "APPLY_PRIM_FRAME";
    case BIND_FRAME: return "BIND_FRAME";
    case BIND_PROPAGATION_FRAME: return "BIND_PROPAGATION_FRAME";
    case EVAL_FRAME: return "EVAL_FRAME";
    case EVAL_LOCAL_VAR_FRAME: return "EVAL_LOCAL_VAR_FRAME";
    case EVAL_OPERANDS_FRAME: return "EVAL_OPERANDS_FRAME";
    case EVAL_OPERATOR_FRAME: return "EVAL_OPERATOR_FRAME";
    case EVAL_SEQUENTIAL_FRAME: return "EVAL_SEQUENTIAL_FRAME";
    case IF_FRAME: return "IF_FRAME";
    default: xassert(FALSE);
  }
  return NULL;
}

static struct frame *alloc_frame(int type)
{
  struct frame *f;
  f = xmalloc(sizeof(struct frame) + sizeof(object) * (frame_size(type) - 1));
  f->type = type;
  return f;
}

static struct frame *fs_top(void)
{
  xassert(sp > 0);
  return fs[sp - 1];
}

static void env_sweep(int depth, void *sym, void *val)
{
  char buf[MAX_STR_LEN];
  printf("	%s:", object_describe(sym, buf));
  printf("%s\n", object_describe(val, buf));
}

static void dump_env(void)
{
  object e;
  printf("-- env\n");
  e = reg[1];
  while (e != object_nil) {
    xassert(typep(e, Env));
    printf("** %p **\n", e);
    xsplay_foreach(&e->env.binding, env_sweep);
    printf("**\n");
    e = e->env.top;
  }
  printf("\n");
}

static void dump_fs(void)
{
  int i, j;
  char buf[MAX_STR_LEN];
  struct frame *f;
  printf("-- register[");
  for (i = 0; i < REG_SIZE; i++) {
    if (i != 0) printf(", ");
    printf("%s", object_describe(reg[i], buf));
  }
  printf("]\n");
  dump_env();
  printf("-- frame stack\n");
  for (i = sp - 1; i >= 0; i--) {
    f = fs[i];
    printf("%s[", frame_name(f->type));
    for (j = 0; j < frame_size(f->type); j++) {
      if (j != 0) printf(", ");
      printf("%s", object_describe(f->local_vars[j], buf));
    }
    printf("]\n");
  }
  printf("\n");
}

static void push_frame(struct frame *f)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) mark_error("stack over flow.");
  else if (sp + 3 > FRAME_STACK_SIZE) xerror("stack over flow.");
  fs[sp] = f;
  sp++;
}

static struct frame *pop_frame(void)
{
  struct frame *top;
  top = fs_top();
  --sp;
  return top;
}

static struct frame *make_apply_frame(object operator)
{
  struct frame *f;
  f = alloc_frame(APPLY_FRAME);
  f->local_vars[0] = operator;
  return f;
}

static struct frame *make_apply_prim_frame(object prim)
{
  struct frame *f;
  f = alloc_frame(APPLY_PRIM_FRAME);
  f->local_vars[0] = prim;
  return f;
}

static struct frame *make_bind_frame(object env, object sym, object val)
{
  struct frame *f;
  f = alloc_frame(BIND_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = sym;
  f->local_vars[2] = val;
  return f;
}

static struct frame *make_bind_propagation_frame(object env, object sym)
{
  struct frame *f;
  f = alloc_frame(BIND_PROPAGATION_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = sym;
  return f;
}

static struct frame *make_eval_frame(object env)
{
  struct frame *f;
  f = alloc_frame(EVAL_FRAME);
  f->local_vars[0] = env;
  return f;
}

static struct frame *make_eval_local_var_frame(object env, object o)
{
  struct frame *f;
  f = alloc_frame(EVAL_LOCAL_VAR_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = o;
  return f;
}

static struct frame *make_eval_operator_frame(object env, object operands)
{
  struct frame *f;
  f = alloc_frame(EVAL_OPERATOR_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = operands;
  return f;
}

static struct frame *make_eval_operands_frame(object env, object operands)
{
  struct frame *f;
  f = alloc_frame(EVAL_OPERANDS_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = operands;
  f->local_vars[2] = object_nil;
  return f;
}

static struct frame *make_eval_sequential_frame(object env, object lis)
{
  struct frame *f;
  f = alloc_frame(EVAL_SEQUENTIAL_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = lis;
  return f;
}

static struct frame *make_if_frame(object env, object args)
{
  struct frame *f;
  f = alloc_frame(IF_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = args;
  return f;
}

// binding stack
struct xarray bs;

static void bs_push(struct frame *f)
{
  xarray_add(&bs, f);
}

static void bs_reset(void)
{
  xarray_reset(&bs);
}

static void bs_reflect(void)
{
  int i;
  for (i = bs.size - 1; i >= 0; i--) push_frame(bs.elt[i]);
}

static void push_eval_operands_frame(object env, object operands)
{
  if (operands == object_nil) reg[0] = object_nil;
  else {
    push_frame(make_eval_operands_frame(env, operands->cons.cdr));
    push_frame(make_eval_frame(env));
    reg[0] = operands->cons.car;
  }
}

static void push_eval_sequential_frame(object env, object lis)
{
  if (lis == object_nil) reg[0] = object_nil;
  else push_frame(make_eval_sequential_frame(env, lis));
}

static object symbol_find(object e, object o)
{
  object result;
  while (e != object_nil) {
    if ((result = xsplay_find(&e->env.binding, o)) != NULL) break;
    e = e->env.top;
  }
  return result;
}

static int valid_keyword_p(object params, object operands)
{
  object p, s;
  while (operands != object_nil) {
    if (!typep(operands->cons.car, Keyword)) {
      mark_error("expected keyword parameter");
      return FALSE;
    }
    p = params;
    while (p != object_nil) {
      s = p->cons.car;
      if (typep(s, Cons)) s = s->cons.car;
      xassert(typep(s, Symbol));
      if (strcmp(s->symbol.name, operands->cons.car->symbol.name + 1) == 0)
        break;
      p = p->cons.cdr;
    }
    if (p == object_nil) {
      mark_error("undeclared keyword parameter");
      return FALSE;
    }
    if ((operands = operands->cons.cdr) == object_nil) {
      mark_error("expected keyword parameter value");
      return FALSE;
    }
    operands = operands->cons.cdr;
  }
  return TRUE;
}

// lambda-list内のシンボルが一意であることのvalidate-lambda-listに追加する。
static void parse_lambda_list(object env, object params, object operands)
{
  object o, pre, k, v, def_v, sup_k;
  struct xsplay supply_tree;
  xsplay_init(&supply_tree, (int(*)(void *, void *))symcmp);
  // parse required parameter
  while (params != object_nil && !typep(params->cons.car, Keyword)) {
    if (operands == object_nil) {
      mark_error("too few arguments");
      return;
    }
    if (!typep(params->cons.car, Cons)) 
      bs_push(make_bind_frame(env, params->cons.car, operands->cons.car));
    else {
      parse_lambda_list(env, params->cons.car, operands->cons.car);
      if (ip_trap_code != TRAP_NONE) return;
    }
    params = params->cons.cdr;
    operands = operands->cons.cdr;
  }
  // parse optional parameter
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil && !typep(params->cons.car, Keyword)) {
      o = params->cons.car;
      def_v = sup_k = NULL;
      if (!typep(o, Cons)) k = o;
      else {
        k = o->cons.car;
        def_v = (o = o->cons.cdr)->cons.car;
        if ((o = o->cons.cdr) != object_nil) sup_k = o->cons.car;
      }
      params = params->cons.cdr;
      if (sup_k != NULL) {
        v = object_bool(operands != object_nil);
        bs_push(make_bind_frame(env, sup_k, v));
      }
      if (operands != object_nil) {
        bs_push(make_bind_frame(env, k, operands->cons.car));
        operands = operands->cons.cdr;
      } else {
        if (def_v == NULL) bs_push(make_bind_frame(env, k, object_nil));
        else {
          bs_push(make_eval_local_var_frame(env, def_v));
          bs_push(make_bind_frame(env, k, NULL));
        }
      }
    }
  }
  // parse rest parameter
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    xarray_add(&bs, make_bind_frame(env, params->cons.car, operands));
    return;
  }
  // parse keyword parameter
  if (params->cons.car == object_key) {
    params = params->cons.cdr;
    if(!valid_keyword_p(params, operands)) return;
    while (params != object_nil) {
      o = params->cons.car;
      v = def_v = sup_k = NULL;
      if (!typep(o, Cons)) k = o;
      else {
        k = o->cons.car;
        def_v = (o = o->cons.cdr)->cons.car;
        if ((o = o->cons.cdr) != object_nil) sup_k = o->cons.car;
      }
      pre = o = operands;
      while (o != object_nil) {
        if (strcmp((o->cons.car->symbol.name + 1), k->symbol.name) == 0) {
          v = o->cons.cdr->cons.car;
          if (o == operands) {
            if (o->cons.cdr != object_nil) operands = o->cons.cdr->cons.cdr;
            else operands = object_nil;
          } else {
            if (o->cons.cdr == object_nil) pre->cons.cdr = object_nil;
            else pre->cons.cdr = o->cons.cdr->cons.cdr;
          }
          break;
        }
        pre = o;
        o = o->cons.cdr->cons.cdr;
      }
      if (sup_k != NULL)
        bs_push(make_bind_frame(env, sup_k, object_bool(v != NULL)));
      if (v != NULL) bs_push(make_bind_frame(env, k, v));
      else {
        if (def_v == NULL) bs_push(make_bind_frame(env, k, object_nil));
        else {
          bs_push(make_eval_local_var_frame(env, def_v));
          bs_push(make_bind_frame(env, k, NULL));
        }
      }
      params = params->cons.cdr;
    }
  }
  if (operands != object_nil) mark_error("too many arguments");
}

static void pop_apply_frame(void)
{
  struct frame *top;
  top = pop_frame();
  reg[1] = gc_new_env(top->local_vars[0]->lambda.env);
  reg[2] = top->local_vars[0];
  xarray_reset(&bs);
  parse_lambda_list(reg[1], reg[2]->lambda.params, reg[0]);
  push_eval_sequential_frame(reg[1], reg[2]->lambda.body);
  bs_reflect();
}

static void pop_apply_prim_frame(void)
{
  struct frame *top;
  int (*prim)(int, object, object *);
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = reg[0];
  prim = xsplay_find(&prim_splay, reg[1]);
  if (!(*prim)(object_length(reg[2]), reg[2], &(reg[0])))
    mark_error("primitive failed");
}

static void pop_bind_frame(void)
{
  struct frame *top;
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];
  reg[3] = top->local_vars[2];
  if (reg[3] != NULL) reg[0] = reg[3];
  xsplay_replace(&reg[1]->env.binding, reg[2], reg[0]);
}

static void pop_bind_propagation_frame(void)
{
  struct frame *top;
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];
  while (TRUE) {
    if (reg[1] == object_nil) {
      xsplay_replace(&object_toplevel->env.binding, reg[2], reg[0]);
      break;
    } else if (xsplay_find(&reg[1]->env.binding, reg[2]) != NULL) {
      xsplay_replace(&reg[1]->env.binding, reg[2], reg[0]);
      break;
    } else reg[1] = reg[1]->env.top;
  }
}

static void pop_eval_frame(void)
{
  reg[1] = pop_frame()->local_vars[0];
  switch (type(reg[0])) {
    case Macro:
    case Lambda:
    case Xint:
    case Xfloat:
    case Keyword:
      return;
    case Symbol:
      reg[2] = reg[0];
      if ((reg[0] = symbol_find(reg[1], reg[0])) == NULL) {
        mark_error("unbind symbol");
        reg[0] = object_nil;
      }
      return;
    case Cons:
      reg[2] = reg[0]->cons.cdr;
      push_frame(make_eval_operator_frame(reg[1], reg[2]));
      push_frame(make_eval_frame(reg[1]));
      reg[0] = reg[0]->cons.car;
      return;
  }
}

static void pop_eval_local_var_frame(void)
{
  reg[0] = fs_top()->local_vars[1];
  pop_eval_frame();
}

static void pop_eval_operator_frame(void)
{
  struct frame *top;
  void (*special)(object, int, object);
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];
  switch (type(reg[0])) {
    case Symbol:
      if ((special = xsplay_find(&special_splay, reg[0])) != NULL) {
        (*special)(reg[1], object_length(reg[2]), reg[2]);
        return;
      }
      if (xsplay_find(&prim_splay, reg[0]) != NULL) {
        push_frame(make_apply_prim_frame(reg[0]));
        push_eval_operands_frame(reg[1], reg[2]);
        return;
      }
      break;
    case Macro:
      push_frame(make_eval_frame(reg[1]));
      push_frame(make_apply_frame(reg[0]));
      reg[0] = reg[2];
      return;
    case Lambda:
      push_frame(make_apply_frame(reg[0]));
      push_eval_operands_frame(reg[1], reg[2]);
      return;
    default: break;
  }
  mark_error("is not a operator");
}

static void pop_eval_operands_frame(void)
{
  struct frame *top;
  top = fs_top();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];
  reg[3] = top->local_vars[2];
  reg[3] = top->local_vars[2] = gc_new_cons(reg[0], reg[3]);
  if (reg[2] == object_nil) {
    pop_frame();
    reg[0] = object_reverse(reg[3]);
  } else {
    push_frame(make_eval_frame(reg[1]));
    reg[0] = reg[2]->cons.car;
    top->local_vars[1] = reg[2]->cons.cdr;
  }
}

static void pop_eval_sequential_frame(void)
{
  struct frame *top;
  top = fs_top();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];
  if (reg[2] == object_nil) pop_frame();
  else {
    push_frame(make_eval_frame(reg[1]));
    reg[0] = reg[2]->cons.car;
    top->local_vars[1] = reg[2]->cons.cdr;
  }
}

static void pop_if_frame(void)
{
  struct frame *top;
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];
  if (reg[0] != object_nil) {
    push_frame(make_eval_frame(reg[1]->cons.car));
    reg[0] = reg[2]->cons.car;
  } else if ((reg[1] = reg[1]->cons.cdr) != object_nil) {
    push_frame(make_eval_frame(reg[1]->cons.car));
    reg[0] = reg[2]->cons.cdr->cons.car;
  } else reg[0] = object_nil;
}

static void trap(void)
{
  printf("ip/error: %s\n", error_message);
  if(ip_trap_code == TRAP_ERROR) dump_fs();
  ip_trap_code=TRAP_NONE;
}

static object eval(object expr)
{
  xassert(sp == 0);
  reg[0] = expr;
  push_frame(make_eval_frame(object_toplevel));
  while (TRUE) {
    xassert(sp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (sp == 0) break;
    switch (fs_top()->type) {
      case APPLY_FRAME: pop_apply_frame(); break;
      case APPLY_PRIM_FRAME: pop_apply_prim_frame(); break;
      case BIND_FRAME: pop_bind_frame(); break;
      case BIND_PROPAGATION_FRAME: pop_bind_propagation_frame(); break;
      case EVAL_FRAME: pop_eval_frame(); break;
      case EVAL_LOCAL_VAR_FRAME: pop_eval_local_var_frame(); break;
      case EVAL_OPERATOR_FRAME: pop_eval_operator_frame(); break;
      case EVAL_OPERANDS_FRAME: pop_eval_operands_frame(); break;
      case EVAL_SEQUENTIAL_FRAME: pop_eval_sequential_frame(); break;
      case IF_FRAME: pop_if_frame(); break;
      default: xassert(FALSE);
    }
    // dump_fs();
  }
  return reg[0];
}

// special forms

// <lambda_list> ::= [<required_params>] 
//                   [:opt <xparams>]
//                   { [:rest <param>] | [:key <xparams>] }
// <required_params> ::= <param> <param> ...
// <xparams> ::= <xparam> <xparam> ...
// <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }

static int valid_xparam_p(object o)
{
  o = o->cons.car;
  if (typep(o, Symbol)) return TRUE;
  return typep(o, Cons) && typep(o->cons.car, Symbol)
    && typep(o = o->cons.cdr, Cons)
    && (o->cons.cdr == object_nil
        || (typep(o = o->cons.cdr, Cons)
          && typep(o->cons.car, Symbol) && o->cons.cdr == object_nil));
}

static int parse_params(object *o)
{
  *o = (*o)->cons.cdr;
  if (!valid_xparam_p(*o)) return FALSE;
  *o = (*o)->cons.cdr;
  while (TRUE) {
    if (*o == object_nil) break;
    if (typep((*o)->cons.car, Keyword)) break;
    if (!valid_xparam_p(*o)) return FALSE;
    *o = (*o)->cons.cdr;
  }
  return TRUE;
}

static int valid_lambda_list_p(int lambda_type, object params)
{
  int type;
  while (TRUE) {
    if (params == object_nil) return TRUE;
    type = type(params->cons.car);
    if (type == Keyword) break;
    else if (type == Symbol) params = params->cons.cdr;
    else if (type == Cons) {
      if (lambda_type != Macro || !valid_lambda_list_p(Macro, params->cons.car))
        return FALSE;
      params = params->cons.cdr;
    }
    else return FALSE;
  }
  if (params->cons.car == object_opt)
    if (!parse_params(&params)) return FALSE;
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    if (!typep(params->cons.car, Symbol)) return FALSE;
    params = params->cons.cdr;
  } else if (params->cons.car == object_key) {
    if (!parse_params(&params)) return FALSE;
  }
  return params == object_nil;
}

// <let> ::= (let (<sym> ...) <expr> ...)
// <sym> -- symbol
// <expr> -- s-expression
SPECIAL(let)
{
  object o;
  if (argc <= 1) {
    mark_error("let: too few argument");
    return;
  }
  if (!listp((o = argv->cons.car))) {
    mark_error("let: argument must be list");
    return;
  }
  env = gc_new_env(env);
  push_eval_sequential_frame(env, argv->cons.cdr);
  while (o != object_nil) {
    if (!typep(o->cons.car, Symbol)) {
      mark_error("let: argument must be symbol");
      return;
    }
    push_frame(make_bind_frame(env, o->cons.car, object_nil));
    o = o->cons.cdr;
  }
}

// <assign> ::= (<- [<sym> <val>] ...)
// <sym> -- symbol
// <val> -- value
SPECIAL(assign)
{
  object s;
  if (argc == 0) return;
  if (argc % 2 != 0) {
    mark_error("<-: must be pair");
    return;
  }
  bs_reset();
  while (argc != 0) {
    s = argv->cons.car;
    if (!typep(s, Symbol)) {
      mark_error("<-: cannot bind except symbol");
    }
    if (s == object_nil) {
      mark_error("<-: cannot bind nil");
    }
    argv = argv->cons.cdr;
    bs_push(make_eval_local_var_frame(env, argv->cons.car));
    bs_push(make_bind_propagation_frame(env, s));
    argv = argv->cons.cdr;
    argc -= 2;
  }
  bs_reflect();
}

SPECIAL(macro)
{
  object sym, params, result;
  if (listp(argv->cons.car)) {
    sym = NULL;
    params = argv->cons.car;
  } else {
    sym = argv->cons.car;
    params = (argv = argv->cons.cdr)->cons.car;
  }
  if (!valid_lambda_list_p(Macro, params))
    mark_error("macro: illegal parameter list");
  result = gc_new_macro(env, params, argv->cons.cdr);
  if (sym != NULL) push_frame(make_bind_frame(env, sym, NULL));
  reg[0] = result;
}

SPECIAL(lambda)
{
  object params;
  params = argv->cons.car;
  if (!valid_lambda_list_p(Lambda, params))
    mark_error("lambda: illegal parameter list");
  reg[0] = gc_new_lambda(env, params, argv->cons.cdr);
}

SPECIAL(quote)
{
  if (argc != 1) {
    if (argc == 0) mark_error("quote: requires argument");
    else mark_error("quote: too many arguments");
  }
  reg[0] = argv->cons.car;
}

SPECIAL(if)
{
  if (argc != 2 && argc != 3) mark_error("if: illegal arguments");
  push_frame(make_if_frame(env, argv->cons.cdr));
  push_frame(make_eval_frame(env));
  reg[0] = argv->cons.car;
}

SPECIAL(begin)
{
  push_eval_sequential_frame(env, argv);
}

void ip_mark(void)
{
  // TODO mark fs.
}

static void init_builtin(void)
{
  int i;
  char *s;
  xsplay_init(&special_splay, (int(*)(void *, void *))symcmp);
  xsplay_init(&prim_splay, (int(*)(void *, void *))symcmp);
  for (i = 0; (s = bi_as_symbol_name(special_name_table[i])) != NULL; i++)
    xsplay_add(&special_splay, gc_new_symbol(s), special_table[i]);
  for (i = 0; (s = bi_as_symbol_name(prim_name_table[i])) != NULL; i++)
    xsplay_add(&prim_splay, gc_new_symbol(s), prim_table[i]);
}

void ip_start(void)
{
  int i;
  char buf[MAX_STR_LEN];
  object o, p;
  init_builtin();
  sp = 0;
  xarray_init(&bs);
  ip_trap_code = TRAP_NONE;
  for (i = 0; i < REG_SIZE; i++) reg[i] = object_nil;
  for (o = object_boot->lambda.body; o != object_nil; o = o->cons.cdr) {
    p = eval(o->cons.car);
    if (VERBOSE_P) printf("%s\n", object_describe(p, buf));
  }
}
