package tfm.examples

import cats.data.Xor

import tfm.{fin, local}

@fin("EncodedSum")
trait SumInterpreter[F[+_, +_]] {
  def left[A, B](a: A): F[A, B]

  def right[A, B](b: B): F[A, B]

  @local def fold[A, B, X](sum: F[A, B])(l: A => X, r: B => X): X
}

object SumInterpreter {
  trait ChurchEncoding[+A, +B] {
    def run[R]: ((A => R), (B => R)) => R
  }

  val encoded: SumInterpreter[ChurchEncoding] =
    new SumInterpreter[ChurchEncoding] {
      def left[A, B](a: A): ChurchEncoding[A, B] =
        new ChurchEncoding[A, B] {
          def run[R]: ((A => R), (B => R)) => R =
            (l, _) => l(a)
        }

      def right[A, B](b: B): ChurchEncoding[A, B] =
        new ChurchEncoding[A, B] {
          def run[R]: ((A => R), (B => R)) => R =
            (_, r) => r(b)
        }

      def fold[A, B, X](sum: ChurchEncoding[A, B])(l: A => X, r: B => X): X =
        sum.run(l, r)
    }


  val xor: SumInterpreter[Xor] =
    new SumInterpreter[Xor] {
      def left[A, B](a: A): Xor[A, B] = Xor.left(a)

      def right[A, B](b: B): Xor[A, B] = Xor.right(b)

      def fold[A, B, X](sum: Xor[A, B])(l: A => X, r: B => X): X =
        sum.fold(l, r)
    }
}

object SumApp extends App {
  import SumInterpreter._
  import SumInterpreter.language._

  def exampleFold[F[+_, +_]](interpreter: SumInterpreter[F], sum: EncodedSum[List[Int], String]): Int = {
    val repr = sum.run(interpreter)
    interpreter.fold(repr)(_.sum, _.size)
  }

  val l = left[List[Int], String](List(1, 2, 3))
  val r = right[List[Int], String]("hello")

  val lei = exampleFold(encoded, l)
  val rei = exampleFold(encoded, r)

  val lxi = exampleFold(xor, l)
  val rxi = exampleFold(xor, r)

  val s =
    s"""
    ${l.run(encoded)}
    ${r.run(encoded)}
    lei = ${lei}
    rei = ${rei}
   
    ${l.run(xor)} 
    ${r.run(xor)} 
    lxi = ${lxi}
    rxi = ${rxi}
    """

  println(s)
}
