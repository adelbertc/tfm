package tfm.examples

import tfm.fin

@fin
trait ExampleInterpreter[F[_]] {
  def lit(n: Int): F[Int]
  def add(lhs: Int, rhs: Int): F[Int]
}

object ExampleInterpreter {
  type Id[A] = A

  val id: ExampleInterpreter[Id] =
    new ExampleInterpreter[Id] {
      def lit(n: Int): Int = n
      def add(lhs: Int, rhs: Int): Int = lhs + rhs
    }
}

object ExampleApp extends App {
  import ExampleInterpreter.language._

  val program = add(3, 4)
  println(program.run(ExampleInterpreter.id))
}
