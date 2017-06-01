/*
  paren writer.
*/

typedef struct {
  FILE *fp;
} Writer;

extern void Writer_init(Writer *writer, FILE *fp);
extern void Writer_write(Writer *writer, S *expr);
