package tfm.examples

import tfm.fin

@fin[ExampleInterpreter]
trait ExampleAlgebra[F[_]] {
  def lit(n: Int): F[Int]
  def add(lhs: Int, rhs: Int): F[Int]
}

object ExampleAlgebra {
  def foo: Int = 42
}

trait ExampleInterpreter[F[_]] extends ExampleAlgebra[F]

object ExampleInterpreter {
  type Id[A] = A

  val id: ExampleInterpreter[Id] =
    new ExampleInterpreter[Id] {
      def lit(n: Int): Int = n
      def add(lhs: Int, rhs: Int): Int = lhs + rhs
    }
}

object ExampleApp extends App {
  import ExampleAlgebra.Example._

  val program = add(3, 4)
  println(program.run(ExampleInterpreter.id))
  println(ExampleAlgebra.foo)
}
