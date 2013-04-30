package scalabo.util.control

import org.scalatest.FreeSpec
import scala.collection.mutable.ArrayBuffer
import org.scalatest.matchers.MustMatchers

class ScopeGuardSpec extends FreeSpec with MustMatchers {
  "ScopeGuardブロックで" - {
    "正常にブロックを抜ける際に、" - {
      "exitで登録された処理は、逆順に実行される" in {
        val order = ArrayBuffer[Int]()

        ScopeGuard { scope =>
          order += 1
          scope exit { order += 2}

          order += 3
          scope exit { order += 4}
        }

        order must be (Seq(1, 3, 4, 2))
      }

      "successで登録された処理は、逆順に実行される" in {
        val order = ArrayBuffer[Int]()

        ScopeGuard { scope =>
          order += 1
          scope success { order += 2 }

          order += 3
          scope success { order += 4}
        }

        order must be (Seq(1, 3, 4, 2))
      }

      "failureで登録された処理は、実行されない" in {
        val order = ArrayBuffer[Int]()

        ScopeGuard { scope =>
          order += 1
          scope failure { order += 2 }

          order += 3
          scope failure { order += 4}
        }

        order must be (Seq(1, 3))
      }
    }

    "例外でブロックを抜ける際に、" - {
      "そのまま外に例外が投げられる" in {
        evaluating {
          ScopeGuard { scope =>
            throw new Exception
          }
        } must produce[Exception]
      }

      "例外発生時点までにexitで登録された処理は、逆順で実行される" in {
        val order = ArrayBuffer[Int]()

        try {
          ScopeGuard { scope =>
            order += 1
            scope exit { order += 2 }

            order += 3
            scope exit { order += 4 }

            throw new Exception

            order += 5
            scope exit { order += 6 }
          }
        } catch {
          case ignored: Exception =>
        }

        order must be (Seq(1, 3, 4, 2))
      }

      "例外発生時点までにsuccessで登録された処理は、実行されない" in {
        val order = ArrayBuffer[Int]()

        try {
          ScopeGuard { scope =>
            order += 1
            scope success { order += 2}

            order += 3
            scope success { order += 4}

            throw new Exception

            order += 5
            scope success { order += 6}
          }
        } catch {
          case ignored: Exception =>
        }

        order must be (Seq(1, 3))
      }

      "例外発生時点までにfailureで登録された処理は、逆順で実行される" in {
        val order = ArrayBuffer[Int]()

        try {
          ScopeGuard { scope =>
            order += 1
            scope failure { order += 2}

            order += 3
            scope failure { order += 4}

            throw new Exception

            order += 5
            scope failure { order += 6}
          }
        } catch {
          case ignored: Exception =>
        }

        order must be (Seq(1, 3, 4, 2))
      }
    }

    "複数の例外が発生する場合は、" - {
      "一番初めに発生した例外が、外に投げられる" in {
        evaluating {
          ScopeGuard { scope =>
            scope exit { throw new SecondException }
            throw new FirstException
          }
        } must produce[FirstException]
      }

      "後から発生した例外は、一番初めに発生した例外のgetSuppressedメソッドで取得できる" in {
        val ex =
          intercept[FirstException] {
            ScopeGuard { scope =>
              scope exit { throw new ThirdException }
              scope exit { throw new SecondException }

              throw new FirstException
            }
          }

        ex.getSuppressed() must have size (2)
      }
    }

    "複数のブロックを抜けるパターンで処理が登録されている場合、パターンに関係なく登録と逆順で処理が実行される" in {
      val order = ArrayBuffer[Int]()
      ScopeGuard { scope =>
        scope exit { order += 1}
        scope success { order += 2}
        scope exit { order += 3}
        scope success { order += 4}
      }

      order must be (Seq(4, 3, 2, 1))
    }

    "登録された処理の実行中に例外が発生した際は、それ以降はexit、failureで登録された処理が実行される" in {
      val order = ArrayBuffer[String]()

      intercept[Exception] {
        ScopeGuard { scope =>
          scope exit { order += "exit1" }
          scope success { order += "success1" }
          scope failure { order += "failure1" }

          scope exit {
            order += "exception"
            throw new Exception
          }

          scope exit { order += "exit2" }
          scope success { order += "success2" }
          scope failure { order += "failure2" }
        }
      }

      order must be (Seq("success2", "exit2", "exception", "failure1", "exit1"))
    }
  }

  private class FirstException extends Exception
  private class SecondException extends Exception
  private class ThirdException extends Exception
}