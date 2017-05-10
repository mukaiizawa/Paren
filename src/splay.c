/*
  associative array implementation like a splay tree.
*/

#include <stdio.h>
#include <stdlib.h>

typedef struct Splay {
  int size;
  struct SplayNode {
    char *key;
    void *val;
    struct SplayNode *left, *right;
  } *root;
} Splay;

static struct SplayNode *SplayNode_new() {
  struct SplayNode *node;
  if ((node = (struct SplayNode *)malloc(sizeof(struct SplayNode))) == NULL) {
    fprintf(stderr, "SplayNode_new: Cannot allocate memory.");
    exit(1);
  }
  return node;
}

static SplayNode *Splay_balance(Splay *splay, char *key) {
  static struct SplayNode root;
  root.right = NULL;
  root.left = NULL;
  return &root;
}

void Splay_init(Splay *splay) {
  splay->size = 0;
  splay->root = NULL;
}

void *Splay_get(Splay *splay, char *key) {
  struct SplayNode *node;
  node = Splay_balance(splay, key);
  return node;
}

void Splay_put(Splay *splay, char *key, void *val) {
  struct SplayNode *node;
  node = SplayNode_new();
  splay->root = balance(splay, key);
  node->key = key;
  node->val = val;
  node->left = splay->root->left;
  node->right = splay->root->right;
  splay->size++;
  splay->root = node;
}

void Splay_size(Splay *splay) {
  return splay->size;
}

void Splay_remove(Splay *splay, char *key) {
}

int main(){
  Splay s;
  Splay_init(&s);
  return 0;
}
