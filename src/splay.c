/*
  associative array implementation like a splay tree.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define sentinel (&sentinelNode)

typedef struct Splay {
  int size;
  struct SplayNode {
    char *key;
    void *val;
    struct SplayNode *left, *right;
  } *root;
} Splay;

static struct SplayNode sentinelNode;

static struct SplayNode *SplayNode_new() {
  struct SplayNode *node;
  if ((node = (struct SplayNode *)malloc(sizeof(struct SplayNode))) == NULL) {
    fprintf(stderr, "SplayNode_new: Cannot allocate memory.");
    exit(1);
  }
  node->left = node->right = sentinel;
  return node;
}

static struct SplayNode *Splaynode_rotR(struct SplayNode *node) {
  struct SplayNode *root;
  root = node->left;
  node->left = root->right;
  root->right = node;
  return root;
}

static struct SplayNode *Splaynode_rotL(struct SplayNode *node) {
  struct SplayNode *root;
  root = node->right;
  node->right = root->left;
  root->left = node;
  return root;
}

static struct SplayNode *Splaynode_rotRR(struct SplayNode *node) {
  return Splaynode_rotR(Splaynode_rotR(node));
}

static struct SplayNode *Splaynode_rotLL(struct SplayNode *node) {
  return Splaynode_rotL(Splaynode_rotL(node));
}

static struct SplayNode *Splaynode_rotLR(struct SplayNode *node) {
  node->left = Splaynode_rotL(node->left); 
  return Splaynode_rotR(node);
}

static struct SplayNode *Splaynode_rotRL(struct SplayNode *node) {
  node->right = Splaynode_rotR(node->right); 
  return Splaynode_rotL(node);
}

static void Splay_balance(Splay *splay, char *key) {
  int i;
  struct SplayNode *root, *work;
  sentinel->key = key;
  sentinel->right = sentinel->left = sentinel;
  root = splay->root;
  while ((i = strcmp(key, root->key)) != 0) {
    if (i < 0) {
      if ((i = strcmp(key, root->left->key)) == 0) {
        root = Splaynode_rotR(root);
        break;
      }
      root = (i < 0)?
        Splaynode_rotRR(root):
        Splaynode_rotLR(root);
    }
    else {
      if ((i = strcmp(key, root->right->key)) == 0) {
        root = Splaynode_rotL(root);
        break;
      }
      root = (i > 0)?
        Splaynode_rotLL(root):
        Splaynode_rotRL(root);
    }
  }
  splay->root = root;
}

static void Splay_rebuild(Splay *splay) {
  struct SplayNode *root, *newRoot;
  if ((root = splay->root)->right == sentinel)
    splay->root = root->left;
  else if ((newRoot = root->left) == sentinel)
    splay->root = root->right;
  else {
    while (newRoot->right != sentinel)
      newRoot = newRoot->right;
    newRoot->right = root->right;
    newRoot->left = root->left;
    splay->root = newRoot;
  }
}

void Splay_init(Splay *splay) {
  splay->size = 0;
  splay->root = sentinel;
}

void *Splay_get(Splay *splay, char *key) {
  Splay_balance(splay, key);
  if (splay->root != sentinel) {
    return splay->root->val;
  }
  Splay_rebuild(splay);
  return NULL;
}

void Splay_put(Splay *splay, char *key, void *val) {
  Splay_balance(splay, key);
  if (splay->root == sentinel) {
    struct SplayNode *newNode;
    splay->size++;
    newNode = SplayNode_new();
    newNode->key = key;
    newNode->left = splay->root->left;
    newNode->right = splay->root->right;
    splay->root = newNode;
  }
  splay->root->val = val;
}

int Splay_size(Splay *splay) {
  return splay->size;
}

void Splay_remove(Splay *splay, char *key) {
}

void dump(struct SplayNode *node, int d) {
  int i;
  if (node == sentinel)
    return;
  for (i = 0; i < d; i++)
    printf("    ");
  printf("%s\n", (char *)node->val);
  if (node->left != sentinel) {
    dump(node->left, d + 1);
  }
  if (node->right != sentinel) {
    dump(node->right, d + 1);
  }
}

int main(){
  Splay s;
  Splay_init(&s);
  Splay_put(&s, "1", "1");
  Splay_put(&s, "2", "2");
  Splay_put(&s, "3", "3");
  Splay_put(&s, "4", "4");
  Splay_put(&s, "5", "5");
  printf("get%s\n", (char *)Splay_get(&s, "4"));
  dump(s.root, 0);
  printf("get%s\n", (char *)Splay_get(&s, "8"));
  dump(s.root, 0);
  printf("get%s\n", (char *)Splay_get(&s, "1"));
  dump(s.root, 0);
  printf("get%s\n", (char *)Splay_get(&s, "5"));
  dump(s.root, 0);
  return 0;
}
