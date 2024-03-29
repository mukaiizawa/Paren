// path and files.

#define PF_ERROR 0
#define PF_NONE 1
#define PF_REGF 2
#define PF_DIR 4
#define PF_OTHER 8
#define PF_READABLE 16
#define PF_WRITABLE 32

#define PF_READABLEFILE (PF_REGF | PF_READABLE)

struct pf_stat {
  int64_t mtime;
  int64_t size;
};

extern int pf_stat(char *fn, struct pf_stat *statbuf);
extern char *pf_exepath(char *argv0, char *path);
extern int pf_utime(char *fn, int64_t mtime);
extern char *pf_getcwd(char *buf);
extern FILE *pf_fopen(char *fn, char *mode);
extern int pf_readdir(char *path, struct xbarray *files);
extern int pf_mkdir(char *pn);
extern int pf_remove(char *fn);
extern int pf_chdir(char *pn);
