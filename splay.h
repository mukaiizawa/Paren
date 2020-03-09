// splay tree.

extern int splay_symcmp(object o, object p);
extern int splay_strcmp(object o, object p);

extern void splay_init(object splay, object key);
extern object splay_find(object splay, object key);
extern void splay_add(object splay, object key, object val);
extern void splay_replace(object splay, object key, object val);

extern void splay_dump(object s);
