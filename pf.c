// path and files.

/* lisence {{{
   Mulk system.
   Copyright (C) 2009-2018 Ken'ichi Tokuoka. All rights reserved.

   Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
}}} */

#include "std.h"

#include <stdlib.h>
#include <string.h>

#if UNIX_P
#include <unistd.h>
#include <dirent.h>
#include <utime.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#define PATH_SEPR '/'
#define PATHS_SEPR ':'
#endif

#if WINDOWS_P
#include <windows.h>
#include <direct.h>
#define PATH_SEPR '\\'
#define PATHS_SEPR ';'
#endif

#if DOS_P
#include <ctype.h>
#include <direct.h>
#include <dos.h>
#include <utime.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#define S_IREAD S_IRUSR
#define S_IWRITE S_IWUSR
#define PATH_SEPR '\\'
#define PATHS_SEPR ';'
#endif

#include "xbarray.h"
#include "pf.h"

#if UNIX_P
static int group_member_p(gid_t gid)
{
	int i,size;
	gid_t *gids;
	
	size=getgroups(0,NULL);
	if(size==-1) xerror("getgroups failed");
	gids=xmalloc(size*sizeof(gid_t));
	if(getgroups(size,gids)==-1) xerror("getgroups failed");
	for(i=0;i<size;i++) {
		if(gid==gids[i]) break;
	}
	xfree(gids);
	return i!=size;
}
#endif

int pf_stat(char *fn,struct pf_stat *pf_statbuf)
{
#if WINDOWS_P
	WIN32_FILE_ATTRIBUTE_DATA attr;
	int result;
	int en;
	
	if(!GetFileAttributesEx(fn,GetFileExInfoStandard,&attr)) {
		en=GetLastError();
		if(en==ERROR_FILE_NOT_FOUND||en==ERROR_PATH_NOT_FOUND
			||en==ERROR_INVALID_NAME) return PF_NONE;
		else return PF_ERROR;
	}
	result=PF_READABLE;
	if(attr.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY) result|=PF_DIR;
	else result|=PF_FILE;
	if((attr.dwFileAttributes&FILE_ATTRIBUTE_READONLY)==0) result|=PF_WRITABLE;
	if(pf_statbuf!=NULL) {
		pf_statbuf->size=((int64_t)attr.nFileSizeHigh<<32)|attr.nFileSizeLow;
		pf_statbuf->mtime=(((int64_t)attr.ftLastWriteTime.dwHighDateTime<<32)
			|attr.ftLastWriteTime.dwLowDateTime)/10000000-11644473600;
	}
	return result;
#else
	int result,type;
	int rbit,wbit;
	struct stat statbuf;

	if(stat(fn,&statbuf)!=0) {
		if(errno==ENOENT) return PF_NONE;
		else return PF_ERROR;
	}

	result=0;
	type=statbuf.st_mode&S_IFMT;
	if(type==S_IFREG) result=PF_FILE;
	else if(type==S_IFDIR) result=PF_DIR;
	else result=PF_OTHER;

#if UNIX_P
	if(statbuf.st_uid==getuid()) {
		rbit=S_IRUSR;
		wbit=S_IWUSR;
	} else if(group_member_p(statbuf.st_gid)) {
		rbit=S_IRGRP;
		wbit=S_IWGRP;
	} else {
		rbit=S_IROTH;
		wbit=S_IWOTH;
	}
#endif

#if WINDOWS_P|DOS_P
	rbit=S_IREAD;
	wbit=S_IWRITE;
#endif

	if(statbuf.st_mode&rbit) result|=PF_READABLE;
	if(statbuf.st_mode&wbit) result|=PF_WRITABLE;

	if(pf_statbuf!=NULL) {
		pf_statbuf->mtime=statbuf.st_mtime;
		pf_statbuf->size=statbuf.st_size;
	}
	return result;	
#endif
}

static int check(char *fn,int mode)
{
	return (pf_stat(fn,NULL)&mode)==mode;
}

static char *find_paths(char *paths,char *name,char *buf)
{
	char *p,*sepr;
	
	p=paths;
	while((sepr=strchr(p,PATHS_SEPR))!=NULL) {
		*sepr='\0';
		xsprintf(buf,"%s/%s",p,name);
		if(check(buf,PF_READABLEFILE)) return buf;
		p=sepr+1;
	}
	xsprintf(buf,"%s/%s",p,name);
	if(check(buf,PF_READABLEFILE)) return buf;
	return NULL;
}

char *pf_exepath(char *argv0,char *path)
{
	char *p,fn[MAX_STR_LEN],*result;
	struct xbarray paths;
	
	strcpy(path,argv0);
#if DOS_P
	return path;	/* djgpp return fullpath, always */
#endif

	p=strrchr(path,PATH_SEPR);
	
#if !UNIX_P
	{
		char *q;
		q=strrchr(path,'.');
		if(q==NULL||(p!=NULL&&q<p)) strcat(path,".exe");
	}
#endif

	if(p!=NULL) return path;

#if !UNIX_P
	if(check(path,PF_READABLEFILE)) return path;
#endif

	strcpy(fn,path);
	xbarray_init(&paths);
	xbarray_adds(&paths,getenv("PATH"));
	xbarray_add(&paths,'\0');
	result=find_paths(paths.elt,fn,path);
	xbarray_free(&paths);
	if(result==NULL) xerror("pf_exepath: can not find");
	return result;
}

#ifdef __DMC__
static int64_t mki64(int hi,int lo)
{
	union {
		struct {
			int lo,hi;
		} i32;
		int64_t i64;
	} u;
	u.i32.hi=hi;
	u.i32.lo=lo;
	return u.i64;
}
#endif

int pf_utime(char *fn,int64_t mtime)
{
#if WINDOWS_P
	HANDLE hFile;
	FILETIME ft;
#ifdef __DMC__
	mtime=mtime*mki64(0,0x989680)+mki64(0x19db1de,0xd53e8000);
#else
	mtime=mtime*10000000+116444736000000000;
#endif
	ft.dwLowDateTime=mtime&0xffffffff;
	ft.dwHighDateTime=mtime>>32;
	hFile=CreateFile(fn,GENERIC_WRITE,0,NULL,OPEN_EXISTING,
		FILE_ATTRIBUTE_NORMAL,NULL);
	if(hFile==INVALID_HANDLE_VALUE) return FALSE;
	if(!SetFileTime(hFile,NULL,NULL,&ft)) return FALSE;
	CloseHandle(hFile);
#else
	struct stat statbuf;
	struct utimbuf utimbuf;

	if(stat(fn,&statbuf)!=0) return FALSE;
	utimbuf.actime=statbuf.st_atime;
	utimbuf.modtime=mtime;

	if(utime(fn,&utimbuf)==-1) return FALSE;
#endif
	return TRUE;
}

char *pf_getcwd(char *buf)
{
	int st;
#if WINDOWS_P
	st=GetCurrentDirectory(MAX_STR_LEN,buf);
	if(st>MAX_STR_LEN) st=0;
#else
	st=getcwd(buf,MAX_STR_LEN)!=NULL;
#endif
	if(!st) xerror("pf_getcwd failed");
	return buf;
}

int pf_readdir(char *path,struct xbarray *dirs)
{
	char *fn;
#if UNIX_P
	{
		DIR *d;
		struct dirent *de;
		
		if((d=opendir(path))==NULL) return FALSE;
		while((de=readdir(d))!=NULL) {
			fn=de->d_name;
			if(strcmp(fn,".")==0||strcmp(fn,"..")==0) continue;
			xbarray_adds(dirs,fn);
			xbarray_add(dirs,'\n');
		}
		closedir(d);
	}
#endif

#if WINDOWS_P
	{
		WIN32_FIND_DATA data;
		HANDLE h;
		char buf[MAX_STR_LEN];
		DWORD err;
		
		xsprintf(buf,"%s\\*",path);
		if((h=FindFirstFile(buf,&data))==INVALID_HANDLE_VALUE) return FALSE;
		while(TRUE) {
			fn=data.cFileName;
			if(!(strcmp(fn,".")==0||strcmp(fn,"..")==0)) {
				xbarray_adds(dirs,fn);
				xbarray_add(dirs,'\n');
			}
			if(FindNextFile(h,&data)==0) {
				err=GetLastError();
				if(err==ERROR_NO_MORE_FILES) break;
				else {
					FindClose(h);
					return FALSE;
				}
			}
		}
		FindClose(h);
	}
#endif

#if DOS_P
	{
		struct find_t find;
		unsigned int rc;
		char buf[MAX_STR_LEN],*p;
		int ch;

		xsprintf(buf,"%s\\*.*",path);
		for(p=buf;(ch=*p)!='\0';p++) {
			if(ch=='/') *p='\\';
		}
		rc=_dos_findfirst(buf,_A_NORMAL|_A_SUBDIR,&find);
		while(rc==0) {
			fn=find.name;
			if(!(strcmp(fn,".")==0||strcmp(fn,"..")==0)) {
				for(p=fn;(ch=*p)!=0;p++) xbarray_add(dirs,tolower(ch));
				xbarray_add(dirs,'\n');
			}
			rc=_dos_findnext(&find);
		}
	}
#endif

	return TRUE;
}

int pf_mkdir(char *path)
{
	int st;
#if WINDOWS_P
	st=CreateDirectory(path,NULL);
#else 
	st=mkdir(path,0777)==0;
#endif
	return st;
}

int pf_remove(char *fn)
{
	int st;
#if WINDOWS_P
	if(check(fn,PF_DIR)) st=RemoveDirectory(fn);
	else st=DeleteFile(fn);
#else
	st=remove(fn)==0;
#endif
	return st;
}

int pf_chdir(char *dir)
{
	int st;
#if WINDOWS_P
	st=SetCurrentDirectory(dir);
#else
#if DOS_P
	{
		unsigned int drive,total;
		drive=tolower(*dir)-'a'+1;
		_dos_setdrive(drive,&total);
		if(!(1<=drive&&drive<=total)) return 0;
	}
#endif
	st=chdir(dir)==0;
#endif
	return st;
}
