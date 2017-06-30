/*
  garbage collector.
  parenでは世代別ガーベッジコレクションを採用する。
  オブジェクトに新たに属性値(数値)'年齢'を持たせる。
  年齢の値によりオブジェクトを次の種類に分類する。
  - 新参 -- 0..2
  - 古参 -- 3
  オブジェクトが生成された瞬間は後述する例外を除き、年齢に0が設定される。
  新参の数がある閾値を超えたタイミングで、新参を対象にGCを行う。
  この閾値を新参GC閾値、このGCを新参GCと呼ぶ。
  新参GC終了後、生存したオブジェクトの年齢をインクリメントする。
  また、一定回数新参GC行った後には古参を対象にGCを行う。
  古参GCを行う頻度をを古参GC実行頻度、このGCを古参GCと呼ぶ。
  永続であるようなオブジェクトはGCの対象としない。
  parenにおいてはkeyword型のオブジェクトと、
  その他のいくつかのグルーバルシンボルがそれにあたる。
  それらのオブジェクトは生成時に年齢に永続値が設定される。
*/

#include <stdio.h>
#include <assert.h>

#include "std.h"
#include "array.h"
#include "stack.h"
#include "splay.h"
#include "env.h"
#include "paren.h"
#include "gc.h"

void GC_init(GC *gc) {
  int size;
  size = sizeof(S *);
  Array_init(&gc->newbies, size, GC_NEWBIE_SIZE);
  Array_init(&gc->elders, size, GC_INIT_ELDER_SIZE);
  Stack_init(&gc->pool);
  gc->times = 0;
}

S *GC_alloc(GC *gc) {
  S *expr;
  if (Stack_isEmpty(&gc->pool)) expr = xmalloc(sizeof(S));
  else expr = Stack_pop(&gc->pool);
  expr->Object.age = GC_NEWBIE;
  return expr;
}

static void GC_mark(Array *objs) {
}

/*
 * 新参を対象にGCを実行する。
 * 参照が生きているオブジェクト以外は解放を行い、
 * それ以外のオブジェクトは年齢をインクリメントする。
 * 古参の閾値に達したオブジェクトは古参に移行する。
 */
static void GC_newbies(GC *gc) {
  // S *newbies;
  // GC_mark(gc->newbies);
  // newbies = gc->newbies;
  // while (!NILP(newbies)) {
  //   if (FIRST(newbies)->Object.isAlive) FIRST(newbies)->Object.isAlive = 0;
  //   else {
  //     S_free(FIRST(newbies));
  //   }
  //   newbies = REST(newbies);
  // }
}

/*
 * 古参を対象にGCを実行する。
 * 新参のGC回数が一定回数になったら実行する。
 */
static void GC_elders(GC *gc) {
  GC_mark(&gc->elders);
}

/*
 * GC実行
 * 新米の数が閾値を超えている場合は実施する。
 * 新米GCを実行した場合は、必要に応じて古参GCを実施する。
 */
void GC_try(GC *gc) {
  // if (gc->newbieNum < GC_MAX_NEWBIE_NUM) return;
  GC_newbies(gc);
  if ((gc->times = (gc->times % GC_FREQ_ELDER) + 1) == 0) GC_elders(gc);
}
