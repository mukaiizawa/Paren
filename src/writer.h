/*
  paren writer.
*/

typedef struct {
  FILE *fp;
} Writer;

extern void Writer_init(Writer *wr, FILE *fp);
extern void Writer_write(Writer *wr, S *expr);
