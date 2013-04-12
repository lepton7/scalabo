package scalabo

package object util extends CloserImplicit {
  import scalabo.util.Managed._

  /**
   * リソースの自動解放を行うためのメソッド
   * 詳しくは[[scalabo.util.Managed]]を参照
   *
   * 使い方1:
   * {{{
   * import scalabo.util._
   *
   * using(new Resource) in { res =>
   *   // リソース使って色々と
   * }
   * }}}
   *
   * 使い方2:
   * {{{
   * import scalabo.util._
   * for(r1 <- using(new Resource);
   *     r2 <- using(new Resource2);
   *     // 自動解放をしたくないリソースは = で定義すれば良い
   *     r3 = new NonCloseResource) {
   *   // リソース使って色々と
   *   ...
   *   // r1, r2は自動的に解放される。
   * }
   * }}}
   */
  def using[R: Closer](resource: R) = Managed.using(resource)
}