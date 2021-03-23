// path and files for unix.

#include "std.h"
#include <stdlib.h>
#include <string.h>
#include <utime.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "pf.h"

#define PATH_SEPR '/'
#define PATHS_SEPR ':'

static int group_member_p(gid_t gid)
{
  int i, size;
  gid_t *gids;
  size = getgroups(0, NULL);
  if (size == -1) xerror("getgroups failed");
  gids = xmalloc(size * sizeof(gid_t));
  if (getgroups(size, gids) == -1) xerror("getgroups failed");
  for (i = 0; i < size; i++) {
    if (gid == gids[i]) break;
  }
  xfree(gids);
  return i != size;
}

int pf_stat(char *fn, struct pf_stat *pf_statbuf)
{
  int rbit, wbit, type, result;
  struct stat statbuf;
  if (stat(fn, &statbuf) != 0) {
    if (errno == ENOENT) return PF_NONE;
    return PF_ERROR;
  }
  result = 0;
  type = statbuf.st_mode & S_IFMT;
  if (type == S_IFREG) result = PF_FILE;
  else if (type == S_IFDIR) result = PF_DIR;
  else result = PF_OTHER;
  if (statbuf.st_uid == getuid()) {
    rbit = S_IRUSR;
    wbit = S_IWUSR;
  } else if (group_member_p(statbuf.st_gid)) {
    rbit = S_IRGRP;
    wbit = S_IWGRP;
  } else {
    rbit = S_IROTH;
    wbit = S_IWOTH;
  }
  if (statbuf.st_mode & rbit) result |= PF_READABLE;
  if (statbuf.st_mode & wbit) result |= PF_WRITABLE;
  if (pf_statbuf != NULL) {
    pf_statbuf->mtime = statbuf.st_mtime;
    pf_statbuf->size = statbuf.st_size;
  }
  return result;	
}

static int check(char *fn, int mode)
{
  return (pf_stat(fn, NULL) & mode) == mode;
}

static char *find_paths(char *paths, char *name, char *buf)
{
  char *p, *sepr;
  p = paths;
  while ((sepr = strchr(p, PATHS_SEPR)) != NULL) {
    *sepr = '\0';
    xsprintf(buf, "%s/%s", p, name);
    if (check(buf, PF_READABLEFILE)) return buf;
    p = sepr + 1;
  }
  xsprintf(buf, "%s/%s", p, name);
  if (check(buf, PF_READABLEFILE)) return buf;
  return NULL;
}

char *pf_exepath(char *argv0, char *path)
{
  char *p, fn[MAX_STR_LEN], *result;
  struct xbarray paths;
  strcpy(path, argv0);
  p = strrchr(path, PATH_SEPR);
  if (p != NULL) return path;
  strcpy(fn, path);
  xbarray_init(&paths);
  xbarray_adds(&paths, getenv("PATH"));
  xbarray_add(&paths, '\0');
  result = find_paths(paths.elt, fn,path);
  xbarray_free(&paths);
  if (result == NULL) xerror("pf_exepath: can not find");
  return result;
}

int pf_utime(char *fn ,int64_t mtime)
{
  struct stat statbuf;
  struct utimbuf utimbuf;
  if (stat(fn, &statbuf) != 0) return FALSE;
  utimbuf.actime = statbuf.st_atime;
  utimbuf.modtime = mtime;
  if (utime(fn, &utimbuf) == -1) return FALSE;
  return TRUE;
}

char *pf_getcwd(char *buf)
{
  if (getcwd(buf, MAX_STR_LEN) == NULL) xerror("pf_getcwd failed");
  return buf;
}

FILE *pf_fopen(char *fn, char *mode)
{
  return fopen(fn, mode);
}

int pf_readdir(char *path, struct xbarray *dirs)
{
  char *fn;
  DIR *d;
  struct dirent *de;
  if ((d = opendir(path)) == NULL) return FALSE;
  while ((de = readdir(d)) != NULL) {
    fn = de->d_name;
    if (strcmp(fn,".") == 0 || strcmp(fn,"..") == 0) continue;
    if (dirs->size != 0) xbarray_add(dirs, '\n');
    xbarray_adds(dirs, fn);
  }
  closedir(d);
  return TRUE;
}

int pf_mkdir(char *path)
{
  return mkdir(path,0777) == 0;
}

int pf_remove(char *fn)
{
  return remove(fn) == 0;
}

int pf_chdir(char *dir)
{
  return chdir(dir) == 0;
}
