// lexical analysis

#include <ctype.h>

#include "std.h"
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

static int digit_val(int ch, int radix)
{
  int result;
  if (isdigit(ch)) result = ch - '0';
  else if (isalpha(ch)) result = tolower(ch) - 'a' + 10;
  else result = -1;
  if (!(0 <= result && result < radix)) lex_error("illegal digit %c", ch);
  return result;
}

static void get_quoted(void)
{
  int quote, val;
  xbarray_reset(&lex_str);
  quote = skip();
  while (next_ch != quote) {
    if (next_ch == EOF) lex_error("quote not closed");
    if (next_ch != '\\') get();
    else {
      skip();
      val = skip();
      switch(next_ch) {
        case 'a': add('\a'); break;
        case 'b': add('\b'); break;
        case 'e': add(0x1b); break;
        case 'f': add('\f'); break;
        case 'n': add('\n'); break;
        case 'r': add('\r'); break;
        case 't': add('\t'); break;
        case 'v': add('\v'); break;
        case 'x': val = digit_val(skip(), 16) * 16;
                  val += digit_val(skip(), 16);
                  add(val);
                  break;
        default: add(val); break;
      }
    }
  }
  skip();
  add('\0');
}

static int identifier_lead_char_p(void)
{
  switch (next_ch) {
    case '$': case '%': case '&': case '*': case '+': case '-': case '/':
    case ':': case '<': case '=': case '>': case '?': case '_': case '.':
      return TRUE;
    default: return isalpha(next_ch);
  }
}

static int identifier_trail_char_p(void)
{
  return identifier_lead_char_p() || isdigit(next_ch);
}

static int lex_number(int sign)
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
    if (sign == -1) lex_fval = sign * lex_fval;
    return LEX_FLOAT;
  }
  if (sign == -1) lex_ival = sign * lex_ival;
  return LEX_INT;
}

static int lex_identifier(int sign)
{
  xbarray_reset(&lex_str);
  if (sign == 1) add('+');
  else if (sign == -1) add('-');
  while (identifier_trail_char_p()) get();
  add('\0');
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
  int sign;
  while (isspace(next_ch)) skip();
  if (next_ch == ';') {
    while (next_ch != '\n') skip();
    skip();
    return lex();
  }
  if (next_ch == EOF || next_ch == '(' || next_ch == ')' || next_ch == '!'
      || next_ch == '\'' || next_ch == '`' || next_ch == ',' || next_ch == '@')
    return skip();
  if (next_ch == '"') {
    get_quoted();
    return lex();    // treat as a comment.
  }
  if (next_ch != '+' && next_ch != '-') sign = 0;
  else {
    if (next_ch == '+') sign = 1;
    else sign = -1;
    skip();
  }
  if (isdigit(next_ch)) return lex_number(sign);
  if (identifier_lead_char_p() || sign != 0) return lex_identifier(sign);
  lex_error("illegal char '%c'", next_ch);
  return -1;
}
