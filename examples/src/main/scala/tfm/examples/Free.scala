package tfm.examples

import cats.{Applicative, Id}
import cats.implicits._

import tfm.fin

@fin("FreeApplicative")
trait ApplicativeInterpreter[F[_]] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def pure[A](a: A): F[A]
}

object ApplicativeInterpreter {
  def applicative[F[_]](implicit F: Applicative[F]): ApplicativeInterpreter[F] =
    new ApplicativeInterpreter[F] {
      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
        F.map2(fa, fb)(f)
      def pure[A](a: A): F[A] = F.pure(a)
    }
}

object ApplicativeApp extends App {
  import ApplicativeInterpreter.applicative
  import ApplicativeInterpreter.language._

  val program = map2(pure(1), pure(2))(_ + _)
  val id = program.run(applicative[Id])
  val list = program.run(applicative[List])
  val option = program.run(applicative[Option])

  val output =
    s"""
    Id = ${id}
    List = ${list}
    Option = ${option}
    """

  println(output)
}
