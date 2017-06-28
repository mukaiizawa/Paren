/*
  splay tree.
*/

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "std.h"
#include "splay.h"

#define sentinel (&sentinelNode)

static struct SplayNode sentinelNode;

static struct SplayNode *SplayNode_new() {
  struct SplayNode *node;
  node = xmalloc(sizeof(struct SplayNode));
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

static struct SplayNode *Splay_balance(Splay *splay, char *key) {
  int cmp;
  struct SplayNode *newRoot;
  sentinel->key = key;
  sentinel->right = sentinel->left = sentinel;
  newRoot = splay->root;
  while ((cmp = splay->cmp(key, newRoot->key)) != 0) {
    if (cmp < 0) {
      if ((cmp = splay->cmp(key, newRoot->left->key)) == 0)
        return splay->root = Splaynode_rotR(newRoot);
      newRoot = (cmp < 0)?
        Splaynode_rotRR(newRoot):
        Splaynode_rotLR(newRoot);
    }
    else {
      if ((cmp = splay->cmp(key, newRoot->right->key)) == 0)
        return splay->root = Splaynode_rotL(newRoot);
      newRoot = (cmp > 0)?
        Splaynode_rotLL(newRoot):
        Splaynode_rotRL(newRoot);
    }
  }
  return splay->root = newRoot;
}

static void Splay_rebuild(Splay *splay) {
  struct SplayNode *newRoot, *node;
  if (splay->root->left == sentinel)
    newRoot = splay->root->right;
  else {
    newRoot = node = splay->root->left;
    while (node->right != sentinel) node = node->right;
    node->right = splay->root->right;
  }
  splay->root = newRoot;
}

static void SplayNode_freeRec(struct SplayNode *node) {
  if (node == sentinel) return;
  SplayNode_freeRec(node->left);
  SplayNode_freeRec(node->right);
  free(node);
}

void Splay_init(Splay *splay, int (*cmp)(void *key1, void *key2)) {
  splay->size = 0;
  splay->root = sentinel;
  splay->cmp = cmp;
}

int Splay_size(Splay *splay) {
  return splay->size;
}

void *Splay_get(Splay *splay, char *key) {
  Splay_balance(splay, key);
  if (splay->root != sentinel) return splay->root->val;
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

void Splay_remove(Splay *splay, char *key) {
  struct SplayNode *rmNode;
  splay->size--;
  Splay_balance(splay, key);
  rmNode = splay->root;
  assert(rmNode != sentinel);
  Splay_rebuild(splay);
  SplayNode_freeRec(rmNode);
}

void Splay_free(Splay *splay) {
  SplayNode_freeRec(splay->root);
}
