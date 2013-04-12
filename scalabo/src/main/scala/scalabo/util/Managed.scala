package scalabo.util

import java.io.Closeable

/**
 * リソースを解放する
 */
trait Closer[-R] {
  def close(r: R)
}

trait CloserImplicit {
  implicit val closeableCloser = new Closer[Closeable] {
    def close(c: Closeable) { c.close() }
  }

  implicit val autoCloseableCloser = new Closer[AutoCloseable] {
    def close(c: AutoCloseable) { c.close() }
  }
}

/**
 * リソースの自動解放を行うためのクラス
 *
 * 使い方1:
 * {{{
 * import scalabo.util.Managed._
 *
 * using(new Resource) in { res =>
 *   // リソースを使って、いろいろやる
 *   ......
 *   // スコープの終わりでリソースは開放される
 * }
 *
 * }}}
 *
 * 使い方2:
 * for式で複数のリソースをまとめて生成
 * {{{
 * import scalabo.util.Managed._
 *
 * for(r1 <- using(new Resource1);
 *     r2 <- using(new Resource2);
 *     // 自動的に開放したくないリソースは unusingを使う
 *     r3 <- unusing(new NonCloseResource) {
 *    // リソース使ってなんやかんや
 *  }
 * }}}
 *
 * リソースの解放はimplicit parameterのCloser[R]が行う。
 * 独自クラスを自動的に開放したい場合は、専用のCloserをimplicitに作ればよい。
 */
object Managed extends CloserImplicit {
  def using[R: Closer](resource: R) = new Managed[R](resource, implicitly[Closer[R]])
  def unusing[R](resource: R) =
    new {
      def foreach[B](action: R => B): Unit = {
        action(resource)
      }
      def flatMap[B](action: R => B): B = action(resource)
      def map[B](action: R => B): B = action(resource)
    }
}

/**
 * リソースを自動的に解放するManagerクラス
 */
class Managed[R] private (resource: R, closer: Closer[_ >: R]) {
  /**
   * リソースを使った処理を行った後に、リソースを開放する。
   */
  def in[B](action: R => B): B = {
    var throwed: Throwable = null
    try {
      action(resource)
    } catch {
      case t: Throwable =>
        throwed = t
        throw t
    } finally {
      try {
        closer.close(resource)
      } catch {
        case t: Throwable =>
          if(throwed != null) {
            throwed.addSuppressed(t)
          } else {
            throw t
          }
      }
    }
  }

  /**
   * for式対応のため追加したメソッド。inメソッドと処理は同じ
   */
  def foreach[B](action: R => B): Unit = in(action)
  /**
   * for式対応のため追加したメソッド。inメソッドと処理は同じ
   */
  def flatMap[B](action: R => B): B = in(action)
  /**
   * for式対応のため追加したメソッド。inメソッドと処理は同じ
   */
  def map[B](action: R => B): B = in(action)
}