/*
  ahead reader.
*/

#ifndef AHDRD
#define AHDRD

#define AHDRD_BUFSIZE 3

typedef struct {
  int pos;
  FILE fp;
  char *buf;
} AheadReader;

extern void ahdrd_init(AheadReader* this , FILE *fp);

#endif
