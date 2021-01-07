// socket.

#include "std.h"

#include <string.h>

#if UNIX_P
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#define xsocket(x, y, z) socket(x, y, z)
#define xclose(x) close(x)
#define xstartup() {}
#define xcleanup() {}
#endif

#if WINDOWS_P
#include <fcntl.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#define xsocket(x, y, z) WSASocket(x, y, z, NULL, 0, 0)
#define xclose(x) closesocket(x)
#define xstartup() \
{ \
  int st; \
  WSADATA data; \
  if ((st = WSAStartup(MAKEWORD(2, 0), &data)) != 0) return FALSE; \
}
#define xcleanup() WSACleanup()
#endif

#include "xsleep.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"
#include "ip.h"

DEFUN(client_socket)
{
  int port, fd;
  char *host, sport[MAX_STR_LEN];
  struct addrinfo hints, *p, *q;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if ((host = bi_string(argv)) == NULL) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &port)) return FALSE;
  xsprintf(sport, "%d", port);
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = 0;
  hints.ai_protocol = 0;
  xstartup();
  if (getaddrinfo(host, sport, &hints, &p) != 0) return FALSE;
  for (q = p; q != NULL; q = q->ai_next) {
    fd = xsocket(q->ai_family, q->ai_socktype, q->ai_protocol);
    if (fd == -1) continue;
    if (connect(fd, q->ai_addr, q->ai_addrlen) != -1) break;
    xclose(fd);
  }
  freeaddrinfo(p);
  if (q == NULL) return FALSE;
  *result = gc_new_xint(fd);
  return TRUE;
}

DEFUN(server_socket)
{
  int port, fd;
  struct sockaddr_in addr;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &port)) return FALSE;
  memset(&addr, 0, sizeof(addr));
  addr.sin_port = htons(port);
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  xstartup();
  if ((fd = xsocket(AF_INET, SOCK_STREAM, 0)) == -1) return FALSE;
  if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) == -1) return FALSE;
  if (listen(fd, 1) == -1) return FALSE;
  *result = gc_new_xint(fd);
  return TRUE;
}

DEFUN(accept)
{
  int sfd, cfd, size;
  struct sockaddr_in addr;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &sfd)) return FALSE;
  size = sizeof(addr);
  if ((cfd = accept(sfd, (struct sockaddr *) &addr, &size)) == -1) return FALSE;
  *result = gc_new_xint(cfd);
  return TRUE;
}

DEFUN(recv)
{
  int fd, from, size;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_arg_type(argv->cons.car, BYTES, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!(0 <= from && from + size <= o->mem.size)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &fd)) return FALSE;
  if ((size = recv(fd, o->mem.elt + from, size, 0)) < 0) return FALSE;
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(send)
{
  int fd, from, size;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_arg_mem(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!(0 <= from && from + size <= o->mem.size)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &fd)) return FALSE;
  if ((size = send(fd, o->mem.elt + from, size, 0)) < 0) return FALSE;
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(closesocket)
{
  int fd;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &fd)) return FALSE;
  xclose(fd);
  xcleanup();
  *result = object_nil;
  return TRUE;
}
