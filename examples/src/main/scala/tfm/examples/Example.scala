package tfm.examples

import tfm.{fin, local}

@fin
trait ExampleInterpreter[F[_]] {
  @local def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def lit(n: Int): F[Int]
  def add(lhs: Int, rhs: Int): F[Int]
}

object ExampleInterpreter {
  type Id[A] = A

  val id: ExampleInterpreter[Id] =
    new ExampleInterpreter[Id] {
      def map2[A, B, C](fa: A, fb: B)(f: (A, B) => C): C =
        f(fa, fb)
      def lit(n: Int): Int = n
      def add(lhs: Int, rhs: Int): Int = lhs + rhs
    }
}

object ExampleApp extends App {
  import ExampleInterpreter.language._

  val program1 = add(3, 4)
  // Doesn't exist
  // map2
  println(program1.run(ExampleInterpreter.id))
}
