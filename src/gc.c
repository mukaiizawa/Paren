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
#include "splay.h"
#include "env.h"
#include "paren.h"
#include "gc.h"

/*
 * GC初期化処理
 */
void GC_init(GC *gc) {
  gc->gcCount = gc->newbieCount = 0;
  gc->newbies = gc->elders = nil;
}

/*
 * マーキング処理
 * GC対象外のオブジェクトにマーキングを行う。
 */
static void GC_mark(S *target) {
  while (!NILP(target)) {
    // do mark
    target = REST(target);
  }
}

/*
 * 新参を対象にGCを実行する。
 * 参照が生きているオブジェクト以外は解放を行い、
 * それ以外のオブジェクトは年齢をインクリメントする。
 * 古参の閾値に達したオブジェクトは古参に移行する。
 */
static void GC_newbies(GC *gc) {
  S *newbies;
  GC_mark(gc->newbies);
  newbies = gc->newbies;
  while (!NILP(newbies)) {
    if (FIRST(newbies)->Object.isAlive) FIRST(newbies)->Object.isAlive = 0;
    else {
      S_free(FIRST(newbies));
    }
    newbies = REST(newbies);
  }
}

/*
 * 古参を対象にGCを実行する。
 * 新参のGC回数が一定回数になったら実行する。
 */
static void GC_elders(GC *gc) {
  GC_mark(gc->elders);
}

/*
 * GCの管理対象を登録する。
 * parenにおいて管理対象となるのは次のオブジェクト
 * 管理対象にならないオブジェクト（永続オブジェクトまたは定数等）
 * - keyword parameter
 * - シンボル(環境のキーになるもの)
 * 管理対象とするもの
 * - 環境の値(環境解放処理にて解放)
 * - 関数の返り値
 */
S *GC_regist(GC *gc, S *expr) {
  assert(expr->Object.age == GC_NEWBIE);
  gc->newbieCount++;
  gc->newbies = Cons_new(expr, gc->newbies);
  return expr;
}

/*
 * GC実行
 * 新米の数が閾値を超えている場合は実施する。
 * 新米GCを実行した場合は、必要に応じて古参GCを実施する。
 */
void GC_try(GC *gc) {
  if (gc->newbieCount < GC_MAX_NEWBIE_NUM) return;
  GC_newbies(gc);
  if ((gc->gcCount = (gc->gcCount % GC_FREQ_ELDER) + 1) == 0) GC_elders(gc);
}
