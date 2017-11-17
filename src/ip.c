// interpreter

int ip_trap_code;

static object cur_process;
static object cur_method;
static object cur_context; /* nil to frame mode */

static int ip;
static int sp;
static int sp_used; /* after last gc */
static int sp_max;
static int fp;
static int cp;
static int cp_used;
static int cp_max;
static int64_t cycle;

void ip_start(object arg,int fs_size,int cs_size)
{
  ip=-1;
  sp=0;
  sp_used=0;
  sp_max=0;
  fp=0;
  cp=0;
  cp_used=0;
  cp_max=0;
  ip_trap_code=TRAP_NONE;
  cycle=0;
  while (ip) {
  }
}

void ip_mark_object(int full_gc_p)
{
}
