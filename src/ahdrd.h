/*
  ahead reader.
*/

#ifndef AHDRD
#define AHDRD

#define AHDRD_BUFSIZE 3

typedef struct {
  int pos;
  FILE *fp;
  char buf[AHDRD_BUFSIZE];
} Ahdrd;

extern void ahdrd_init(Ahdrd* this , FILE *fp);
extern char ahdrd_getch(Ahdrd* this);
extern char ahdrd_nextch(Ahdrd* this);

#endif
