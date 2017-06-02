/*
  paren writer.
*/

typedef struct {
  FILE *fp;
} Writer;

extern void Writer_init(Writer *wr, FILE *fp);
extern void Writer_write(Writer *wr, S *expr);
extern FILE *Writer_getFp(Writer *wr);
extern void Writer_setFp(Writer *wr, FILE *fp);
