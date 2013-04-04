package scalabo.util

object ExitType extends Enumeration {
  val EXIT, SUCCESS, FAILURE = Value
}

class Procedure(val exit: ExitType.Value, val proc: ()=>Any)

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
 *   stmt.execute
 *   db.commmit
 * }
 * }}}
 * 
 * 上記のコードは以下とほぼ等価になる
 * {{{
 * val db = DB.open("some connection string")
 * 
 * try {
 *   val stmt = db.createStatement("update --- set ------")
 *   
 *   try {
 *     stmt.execute
 *     
 *     db.commit
 *   } catch {
 *     case e: Throwable => {
 *       db.rollback
 *       throw e
 *     }
 *   }finally {
 *     try {
 *       stmt.close
 *     } catch {
 *       case ignored: Throwable =>
 *     }
 *   }
 * } finally {
 *   try {
 *     db.close
 *   } catch {
 *     case ignored: Throwable =>
 *   }
 * }
 * }}}
 */
object ScopeGuard {
  def apply(block: ScopeGuard => Any) = {
    val scope = new ScopeGuard
    var exitType = ExitType.FAILURE
    try {
      block(scope)
      exitType = ExitType.SUCCESS
    } finally {
      def safeCall(p: Procedure) {
        try {
          p.proc()
        } catch {
          case ignored: Throwable =>
        }
      }
      scope.procs.reverseIterator.filter(p => p.exit == ExitType.EXIT || p.exit == exitType).foreach(safeCall)
    }
  }
}

class ScopeGuard private {

  import scala.collection.mutable.ArrayBuffer

  private val procs = ArrayBuffer[Procedure]()

  def exit(proc: => Any): Unit = {
    procs += new Procedure(ExitType.EXIT, () => proc)
  }

  def success(proc: => Any): Unit = {
    procs += new Procedure(ExitType.SUCCESS, () => proc)
  }

  def failure(proc: => Any): Unit = {
    procs += new Procedure(ExitType.FAILURE, () => proc)
  }
}