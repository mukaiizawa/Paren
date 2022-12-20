// socket.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"
#include "ip.h"

#if WINDOWS_P
#define SHUT_RD SD_RECEIVE
#define SHUT_WR SD_SEND
#define SHUT_RDWR SD_BOTH
#define socket(x, y, z) WSASocket(x, y, z, NULL, 0, 0)
#define close(x) closesocket(x)
#endif

DEFUN(sock_2e__5f_startup)
{
#if WINDOWS_P
  int st;
  WSADATA data;
  if ((st = WSAStartup(MAKEWORD(2, 0), &data)) != 0) return ip_sigerr(OSError, "WSAStartup failed");
#endif
  return TRUE;
}

DEFUN(sock_2e__5f_cleanup)
{
#if WINDOWS_P
  WSACleanup();
#endif
  return TRUE;
}

DEFUN(gethostname)
{
  char buf[MAX_STR_LEN];
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  if (gethostname(buf, MAX_STR_LEN) != 0) return ip_sigerr(OSError, "gethostname failed");
  *result = gc_new_mem_from_cstr(STRING, buf);
  return TRUE;
}

DEFUN(client_2d_socket)
{
  int port, fd;
  char *host, sport[MAX_STR_LEN];
  struct addrinfo hints, *p, *q;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstring(argv->cons.car, &host)) return FALSE;
  if (!bi_cint(argv->cons.cdr->cons.car, &port)) return FALSE;
  xsprintf(sport, "%d", port);
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = 0;
  hints.ai_protocol = 0;
  if (getaddrinfo(host, sport, &hints, &p) != 0) return ip_sigerr(OSError, "getaddrinfo failed");
  for (q = p; q != NULL; q = q->ai_next) {
    if ((fd = socket(q->ai_family, q->ai_socktype, q->ai_protocol)) == -1) continue;
    if (connect(fd, q->ai_addr, q->ai_addrlen) != -1) break;
    close(fd);
  }
  freeaddrinfo(p);
  if (q == NULL) return ip_sigerr(OSError, "connect failed");
  *result = gc_new_xint(fd);
  return TRUE;
}

DEFUN(server_2d_socket)
{
  int port, fd;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cint(argv->cons.car, &port)) return FALSE;
  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_port = htons(port);
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  if ((fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) return ip_sigerr(OSError, "socket failed");
  if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) == -1) return ip_sigerr(OSError, "bind failed");
  if (listen(fd, 1) == -1) return ip_sigerr(OSError, "listen failed");
  *result = gc_new_xint(fd);
  return TRUE;
}

DEFUN(accept)
{
  int sfd, cfd;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cint(argv->cons.car, &sfd)) return FALSE;
  struct sockaddr_in addr;
  socklen_t size = sizeof(addr);
  if ((cfd = accept(sfd, (struct sockaddr *) &addr, &size)) == -1) return ip_sigerr(OSError, "accept failed");
  *result = gc_new_xint(cfd);
  return TRUE;
}

DEFUN(recv)
{
  int fd, from, size;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_argv(BI_BYTES, argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, from + size, o->mem.size)) return FALSE;
  if (!bi_cint(argv->cons.cdr->cons.car, &fd)) return FALSE;
  if ((size = recv(fd, o->mem.elt + from, size, 0)) < 0) return ip_sigerr(OSError, "recv failed");
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(send)
{
  int fd, from, size;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM, argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, from + size, o->mem.size)) return FALSE;
  if (!bi_cint(argv->cons.cdr->cons.car, &fd)) return FALSE;
  if ((size = send(fd, o->mem.elt + from, size, 0)) < 0) return ip_sigerr(OSError, "send failed");
  *result = gc_new_xint(size);
  return TRUE;
}

static int how_table[] = { SHUT_RD, SHUT_WR, SHUT_RDWR };

DEFUN(shutdown)
{
  int fd, how;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cint(argv->cons.car, &fd)) return FALSE;
  if (!bi_cint(argv->cons.cdr->cons.car, &how)) return FALSE;
  if (!bi_range(0, how, 2)) return FALSE;
  shutdown(fd, how_table[how]);
  *result = object_nil;
  return TRUE;
}

DEFUN(close)
{
  int fd;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cint(argv->cons.car, &fd)) return FALSE;
  close(fd);
  *result = object_nil;
  return TRUE;
}
