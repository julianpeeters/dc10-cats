# dc10-cats
Library for use with the `dc10-scala` code generator

### Getting Started
 - Library for Scala @SCALA@ (JS, JVM, and Native platforms)

```scala
"com.julianpeeters" %% "dc10-cats" % "@VERSION@"
```

### Usage

Use the dsl to define scala code that depends on cats:

```scala mdoc
import dc10.cats.dsl.*
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g., "Hello, World!"

val snippet =
  for
    _ <- VAL("msg", OPTION(STRING), Option(0).AS("Hello, World!"))
    _ <- VAL("num", OPTION(INT), Option(0).MAP(VAL("i", INT) ==> (i => i)))
  yield ()
```

Use the compiler in `dc10-scala` to render code `toString` or `toVirtualFile`:

```scala mdoc
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.4.0`

val result: String = snippet.compile.toString["scala-3.4.0"]
```