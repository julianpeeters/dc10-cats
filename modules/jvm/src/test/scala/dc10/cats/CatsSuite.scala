
import dc10.cats.dsl.*
import dc10.scala.compiler.{compile, toString}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.4.0`
import scala.language.implicitConversions

import munit.FunSuite

class CatsSuite extends FunSuite:

  test("functor"):

    def ast =
      for
        _ <- VAL("msg", OPTION(STRING), Option(0).AS("Howdy!"))
        _ <- VAL("num", OPTION(INT), Option(0).MAP(VAL("i", INT) ==> (i => i)))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|val msg: Option[String] = Option(0).as("Howdy!")
         |val num: Option[Int] = Option(0).map(i => i)""".stripMargin
      
    assertEquals(obtained, expected)