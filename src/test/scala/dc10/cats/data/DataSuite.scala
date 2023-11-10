package dc10.cats.data

import dc10.cats.data.dsl.*
import dc10.scala.compiler.{compile, toString}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.1`
import scala.language.implicitConversions

import munit.FunSuite

class DataSuite extends FunSuite:

  test("functor"):

    def ast =
      for
        _ <- VAL("s", STORE(INT, STRING))
        _ <- VAL("store", STORE(INT, STRING),
          Store(
            VAL("_", INT) ==> (_ => "hello"),
            0
          )
        )
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.1"]
      
    val expected: String =
      """|val s: Store[Int, String]
         |val store: Store[Int, String] = Store(_ => "hello", 0)""".stripMargin
      
    assertEquals(obtained, expected)