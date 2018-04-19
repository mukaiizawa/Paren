// lexical analysis

#include <ctype.h>

#include "std.h"
#include "lex.h"

/*
 * paren bnf
 * <s_expr> ::= <list> | <atom>
 * <list> ::= <pure_list> | <dot_list>
 * <pure_list> ::= '(' [<s_expr>] ... ')'
 * <dot_list> ::= '(' <s_expr> ... '.' <s_expr> ')'
 * <atom> ::= <number> | <symbol> | <keyword>
 * <number> ::= [<digit>+ 'x'] [0-9a-z]+ | <digit>+ [ '.' <digit>+ ]
 * <symbol> ::= <identifier>
 * <keyword> ::= ':' <identifier>
 * <identifier> ::= <identifier_first> [<identifier_rest>] ...
 * <identifier_first> ::= [!$%&*+-/\-<=>?a-zA-Z_]
 * <identifier_rest> ::= <identifier_first> | [0-9]
 * <digit> ::= [0-9]
 */

struct xbarray lex_str;
int lex_ival;
double lex_fval;

static FILE *fp;
static int line;
static int column;

static int next_ch;

void lex_error(char *fmt, ...)
{
  char buf[MAX_STR_LEN];
  va_list va;
  va_start(va, fmt);
  xvsprintf(buf, fmt, va);
  va_end(va);
  xerror("%s at line: %d, column: %d.", buf, line, column);
}

static int skip(void)
{
  int result;
  result = next_ch;
  if (result == '\n') {
    line++;
    column = 0;
  } else column++;
  next_ch = fgetc(fp);
  return result;
}

static void add(int ch)
{
  xbarray_add(&lex_str, ch);
}

static int get(void)
{
  int ch;
  ch = skip();
  add(ch);
  return ch;
}

void lex_start(FILE *infp)
{
  fp = infp;
  line = 1;
  column = 0;
  xbarray_init(&lex_str);
  skip();
}

static int digit_val(int ch, int radix)
{
  int result;
  if (isdigit(ch)) result = ch - '0';
  else if (isalpha(ch)) result = tolower(ch) - 'a' + 10;
  else result = -1;
  if (!(0 <= result && result < radix)) lex_error("illegal digit %c", ch);
  return result;
}

static int identifier_lead_char_p(void)
{
  switch (next_ch) {
    case '!': case '$': case '%': case '&': case '*': case '+': case '/':
    case '-': case ':': case '<': case '=': case '>': case '?': case '_':
      return TRUE;
    default: return isalpha(next_ch);
  }
}

static int identifier_trail_char_p(void)
{
  return identifier_lead_char_p() || isdigit(next_ch);
}

int lex(void)
{
  int radix;
  double factor;
  while (isspace(next_ch)) skip();
  if (next_ch == ';') {
    while (next_ch != '\n') skip();
    skip();
    return lex();
  }
  if (next_ch == EOF || next_ch == '(' || next_ch == ')' || next_ch == '.')
    return skip();
  if (isdigit(next_ch)) {
    lex_ival = 0;
    while (isdigit(next_ch)) lex_ival = lex_ival * 10 + digit_val(skip(), 10);
    if (next_ch == 'x') {
      skip();
      radix = lex_ival;
      lex_ival = 0;
      if (radix == 0) radix = 16;
      while (isalnum(next_ch))
        lex_ival = lex_ival * radix + digit_val(skip(), radix);
    } else if (next_ch == '.') {
      skip();
      lex_fval = lex_ival;
      factor = 0.1;
      while (isdigit(next_ch)) {
        lex_fval += factor * digit_val(skip(), 10);
        factor /= 10;
      }
      return LEX_FLOAT;
    }
    return LEX_INT;
  } else if (identifier_lead_char_p()) {
    xbarray_reset(&lex_str);
    while (identifier_trail_char_p()) get();
    add('\0');
    return LEX_SYMBOL;
  }
  lex_error("illegal char '%c'", next_ch);
  return 0;
}
