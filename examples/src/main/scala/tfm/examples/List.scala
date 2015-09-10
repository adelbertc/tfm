package tfm.examples

import cats.Monoid
import cats.implicits._

import tfm.fin

@fin(algebraName = "EncodedList")
trait ListInterpreter[F[_]] {
  def nil[A]: F[A]
  def cons[A](head: A, tail: F[A]): F[A]
}

object ListInterpreter {
  type FoldMap[A] = { type L[X] = (X => A) => A }

  def foldMap[B](implicit B: Monoid[B]): ListInterpreter[FoldMap[B]#L] =
    new ListInterpreter[FoldMap[B]#L] {
      def nil[A]: (A => B) => B = _ => B.empty
      def cons[A](head: A, tail: (A => B) => B): (A => B) => B =
        (map: A => B) => B.combine(map(head), tail(map))
    }

  type FoldRight[A] = { type L[X] = A => ((X, A) => A) => A }

  def foldRight[B]: ListInterpreter[FoldRight[B]#L] =
    new ListInterpreter[FoldRight[B]#L] {
      def nil[A]: B => ((A, B) => B) => B = b => _ => b
      def cons[A](head: A, tail: B => ((A, B) => B) => B): B => ((A, B) => B) => B =
        (n: B) => (c: (A, B) => B) => c(head, tail(n)(c))
    }
}

object MonoidApp extends App {
  import ListInterpreter._
  import ListInterpreter.language._

  val program1 = nil[Int]
  val result1 = program1.run[FoldMap[Int]#L](foldMap[Int])(identity) // 0

  val program2 = cons(1, cons(2, cons(3, nil)))
  val result2 = program2.run[FoldMap[Int]#L](foldMap[Int])(identity) // 6

  val result3 = program2.run[FoldMap[List[Int]]#L](foldMap[List[Int]])(List(_)) // List(1, 2, 3)

  val result4 = program2.run[FoldRight[List[Int]]#L](foldRight[List[Int]])(List.empty)(_ :: _) // List(1, 2, 3)

  // Check if 2 exists in the list
  val result5 = program2.run[FoldRight[Boolean]#L](foldRight[Boolean])(false)((x, b) => b || (x == 2))

  println(s"result1 = ${result1}\nresult2 = ${result2}\nresult3 = ${result3}\nresult4 = ${result4}\nresult5 = ${result5}")
}
