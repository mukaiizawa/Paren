// socket.

#include "std.h"

#include <string.h>

#if UNIX_P
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#define xclose(x) close(x)
#define xstart_up() {}
#define xclean_up() {}
#endif

#if WINDOWS_P
#include <winsock2.h>
#include <ws2tcpip.h>
#define xclose(x) closesocket(x)
#define start_up() \
{ \
  int st; \
  WSADATA data; \
  if ((st = WSAStartup(MAKEWORD(2, 0), &data)) != 0) { \
    printf("%d\n", st); \
    return FALSE; \
  } \
}
#define xclean_up() WSACleanup()
#endif

#include "xsleep.h"
#include "xbarray.h"
#include "xarray.h"
#include "splay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"
#include "ip.h"

DEFUN(server_socket)
{
  int port, fd;
  struct sockaddr_in addr;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &port)) return FALSE;
  memset(&addr, 0, sizeof(addr));
  addr.sin_port = htons(port);
  addr.sin_family = AF_UNSPEC;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  start_up();
  fd = socket(AF_UNSPEC, SOCK_STREAM, 0);
  bind(fd, (struct sockaddr *)&addr, sizeof(addr));
  listen(fd, 1);
  return TRUE;
}

DEFUN(client_socket)
{
  int fd;
  char *addr[2];
  struct addrinfo hints, *p, *q;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_strings(2, argv, addr)) return FALSE;
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = 0;
  hints.ai_protocol = 0;
  start_up();
  if (getaddrinfo(addr[0], addr[1], &hints, &p) != 0) return FALSE;
  for (q = p; q != NULL; q = q->ai_next) {
    fd = socket(q->ai_family, q->ai_socktype, q->ai_protocol);
    if (fd == -1) continue;
    if (connect(fd, q->ai_addr, q->ai_addrlen) != -1) break;
    xclose(fd);
  }
  freeaddrinfo(p);
  if (q == NULL) return FALSE;
  *result = gc_new_xint(fd);
  return TRUE;
}

DEFUN(closesocket)
{
  int fd;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &fd)) return FALSE;
  xclose(fd);
  xclean_up();
  return TRUE;
}
