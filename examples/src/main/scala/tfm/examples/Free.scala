package tfm.examples

import cats.{Applicative, Id, Monoid}
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

@fin("FreeMonoid")
trait MonoidInterpreter[F[_]] {
  def nil[A]: F[A]
  def cons[A](head: A, tail: F[A]): F[A]
}

object MonoidInterpreter {
  type FoldMap[A] = { type F[X] = (X => A) => A }

  def foldMap[B](implicit B: Monoid[B]): MonoidInterpreter[FoldMap[B]#F] =
    new MonoidInterpreter[FoldMap[B]#F] {
      def nil[A]: (A => B) => B = _ => B.empty
      def cons[A](lhs: A, rhs: (A => B) => B): (A => B) => B =
        (map: A => B) => B.combine(map(lhs), rhs(map))
    }
}

object MonoidApp extends App {
  import MonoidInterpreter._
  import MonoidInterpreter.language._

  val program1 = nil[Int]
  val result1 = program1.run[FoldMap[Int]#F](foldMap[Int])(identity) // 0

  val program2 = cons(1, cons(2, cons(3, nil)))
  val result2 = program2.run[FoldMap[Int]#F](foldMap[Int])(identity) // 6

  val result3 = program2.run[FoldMap[List[Int]]#F](foldMap[List[Int]])(List(_)) // List(1, 2, 3)

  println(s"result1 = ${result1}\nresult2 = ${result2}\nresult3 = ${result3}")
}
