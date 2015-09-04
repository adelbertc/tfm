# tfm
tfm is "tagless final macro" - the project is intended to eliminate the boilerplate
associated with setting up an EDSL encoded in the [Tagless](http://okmij.org/ftp/tagless-final/index.html)
approach.

## Example

```scala
/** EDSL side */
import tfm.tfm

@tfm[Interpreter]
trait FooAlgebra[F[_]] {
  def foo(x: Int): F[String]
  val y: F[Double]
}

trait Interpreter[F[_]] extends FooAlgebra[F]

/** User side */
scala> type Id[A] = A
defined type alias Id

scala> new Interpreter2[Id] { def foo(x: Int): String = x.toString; val y: Double = 3.14 }
res2: Interpreter2[Id] = $anon$1@38c90220

scala> Foo.foo(5)
res3: Foo[String] = Foo$$anon$1@34f24c6c

scala> res3.run(res2)
res4: Id[String] = 5
```

### License
Code is provided under the MIT license available at http://opensource.org/licenses/MIT, as well as the
LICENSE file.
