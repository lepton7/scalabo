package scalabo.util

object ExitType extends Enumeration {
  val EXIT, SUCCESS, FAILURE = Value
}

class Procedure(exit: ExitType.Value, proc: =>Any) {
  def call(throwed: Option[Throwable]): Option[Throwable] = {
    try {
      ((exit, throwed): @unchecked) match {
        case (ExitType.EXIT,    _)       => proc
        case (ExitType.SUCCESS, None)    => proc
        case (ExitType.FAILURE, Some(e)) => proc
      }
      throwed
    } catch {
      case e: Throwable =>
        throwed match {
          case None => Some(e)
          case Some(ex) =>
            ex.addSuppressed(e)
            Some(ex)
        }
    }
  }
}

/**
 * D言語のスコープガード機能
 *
 * サンプル：
 * {{{
 * ScopeGuard { scope =>
 *   val db = DB.open("some connection string")
 *   scope exit { db.close }
 *
 *   val stmt = db.createStatement("update --- set ------")
 *   scope exit { stmt.close }
 *
 *   scope failure { db.rollback }
 *   scope success { db.commit }
 *   stmt.execute
 * }
 * }}}
 *
 * 上記のコードは以下とほぼ等価になる。
 * 実際にはJava7のtry-with-resourcesと同じように、
 * 複数例外が発生した場合に、後から発生した例外をsuppressedExceptionとして追加する処理が入っている。
 * ただコードで書くと煩雑なので省略
 * {{{
 * val db = DB.open("some connection string")
 *
 * try {
 * val stmt = db.createStatement("update --- set ------")
 *   try {
 *     stmt.execute
 *
 *     db.commit
 *   } catch {
 *     case e: Throwable => {
 *       db.rollback
 *       throw e
 *     }
 *   } finally {
 *     stmt.close
 *   }
 * } finally {
 *   db.close
 * }
 * }}}
 */
object ScopeGuard {
  def apply(block: ScopeGuard => Any) = {
    val scope = new ScopeGuard
    var throwed: Option[Throwable] = None
    try {
      block(scope)
    } catch {
      case e: Throwable => throwed = Some(e)
    } finally {
      throwed = scope.procs.reverseIterator.foldLeft(throwed)((t, p) => p.call(t))
    }

    (throwed: @unchecked) match {
      case Some(e) => throw e
    }
  }
}

class ScopeGuard private {

  import scala.collection.mutable.ArrayBuffer

  private val procs = ArrayBuffer[Procedure]()

  def exit(proc: => Any): Unit = {
    procs += new Procedure(ExitType.EXIT, proc)
  }

  def success(proc: => Any): Unit = {
    procs += new Procedure(ExitType.SUCCESS, proc)
  }

  def failure(proc: => Any): Unit = {
    procs += new Procedure(ExitType.FAILURE, proc)
  }
}