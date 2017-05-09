/*
  symple splay tree.
*/

#include <stdlib.h>

typedef struct Splay {
  char *key;
  void *val;
  struct Splay *left, *right;
} Splay;

Splay *Splay_new() {
  Splay *splay;
  if ((splay = (splay *)calloc(1, sizeof(struct splay))) == NULL) {
    fprintf(stderr, "Splay_new: Cannot allocate memory.");
    exit(1);
  }
  return splay;
}


int main(){
  Splay *s;
}
