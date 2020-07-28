// lexical analysis

#include "std.h"

#include <ctype.h>

#include "lex.h"

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

static int lex_comment(void)
{
  while (next_ch != '\n') skip();
  skip();
  return lex();
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

static int lex_number(void)
{
  int radix;
  double factor;
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
}

static int lex_string(void)
{
  int quote, val;
  quote = skip();
  while (next_ch != quote) {
    if (next_ch == EOF) lex_error("quote not closed");
    if (next_ch != '\\') get();
    else {
      skip();
      switch (next_ch) {
        case 'a': skip(); add('\a'); break;
        case 'b': skip(); add('\b'); break;
        case 'e': skip(); add(0x1b); break;
        case 'f': skip(); add('\f'); break;
        case 'n': skip(); add('\n'); break;
        case 'r': skip(); add('\r'); break;
        case 't': skip(); add('\t'); break;
        case 'v': skip(); add('\v'); break;
        case 'x':
          skip();
          val = digit_val(skip(), 16) * 16;
          val += digit_val(skip(), 16);
          add(val);
          break;
        default: get(); break;
      }
    }
  }
  skip();
  return LEX_STRING;
}

static int identifier_symbol_alpha_p(void)
{
  switch (next_ch) {
    case '!':
    case '#':
    case '$':
    case '%':
    case '&':
    case '*':
    case '.':
    case '/':
    case '<':
    case '=':
    case '>':
    case '?':
    case '[':
    case ']':
    case '^':
    case '_':
    case '{':
    case '|':
    case '}':
      return TRUE;
    default:
      return isalpha(next_ch);
  }
}

static int identifier_sign_p(void)
{
  switch (next_ch) {
    case '+':
    case '-':
      return TRUE;
    default:
      return FALSE;
  }
}

static int identifier_trail_char_p(void)
{
  return identifier_symbol_alpha_p() || isdigit(next_ch) || identifier_sign_p();
}

static void lex_identifier_sign(void)
{
  if (identifier_sign_p() || identifier_symbol_alpha_p())
    while (identifier_trail_char_p()) get();
}

static void lex_identifier(void)
{
  if (identifier_sign_p()) {
    get();
    lex_identifier_sign();
    return;
  }
  if (identifier_symbol_alpha_p()) {
    while (identifier_trail_char_p()) get();
    return;
  }
  lex_error("illegal identifier");
}

static int lex_symbol(void)
{
  lex_identifier();
  return LEX_SYMBOL;
}

static int lex_keyword(void)
{
  skip();
  lex_identifier();
  return LEX_KEYWORD;
}

static int lex_sign(void)
{
  int sign, token_type;
  sign = get();
  if (isdigit(next_ch)) {
    token_type = lex_number();
    if (sign == '-') {
      if (token_type == LEX_INT) lex_ival *= -1;
      else lex_fval *= -1;
    }
    return token_type;
  }
  lex_identifier_sign();
  return LEX_SYMBOL;
}

char *lex_token_name(char *buf, int token)
{
  char *name;
  switch(token) {
    case LEX_SYMBOL: name="symbol"; break;
    case EOF: name = "EOF"; break;
    default: name = NULL; break;
  }
  if(name != NULL) strcpy(buf, name);
  else xsprintf(buf, "%c", token);
  return buf;
}

int lex(void)
{
  xbarray_reset(&lex_str);
  while (isspace(next_ch)) skip();
  switch (next_ch) {
    case EOF:
    case '(':
    case ')':
    case '\'':
      return skip();
    case ';':
      return lex_comment();
    case '"':
      return lex_string();
    case ':':
      return lex_keyword();
    case '+':
    case '-':
      return lex_sign();
    default:
      if (isdigit(next_ch)) return lex_number();
      return lex_symbol();
  }
}
