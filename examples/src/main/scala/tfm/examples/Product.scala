package tfm.examples

import tfm.{fin, local}

@fin("EncodedProduct", "ProductReader")
trait ProductInterpreter[F[_, _]] {
  def pair[A, B](a: A, b: B): F[A, B]

  def fst[A, B](pair: F[A, B]): A

  def snd[A, B](pair: F[A, B]): B
}

object ProductInterpreter {
  trait ChurchEncoding[A, B] {
    def run[R]: ((A, B) => R) => R
  }

  val encoded: ProductInterpreter[ChurchEncoding] =
    new ProductInterpreter[ChurchEncoding] {
      def pair[A, B](a: A, b: B): ChurchEncoding[A, B] =
        new ChurchEncoding[A, B] {
          def run[R]: ((A, B) => R) => R = f => f(a, b)
        }

      def fst[A, B](pair: ChurchEncoding[A, B]): A =
        pair.run[A]((a, _) => a)

      def snd[A, B](pair: ChurchEncoding[A, B]): B =
        pair.run[B]((_, b) => b)
    }


  val tuple: ProductInterpreter[Tuple2] =
    new ProductInterpreter[Tuple2] {
      def pair[A, B](a: A, b: B): (A, B) = (a, b)

      def fst[A, B](pair: (A, B)): A = pair._1

      def snd[A, B](pair: (A, B)): B = pair._2
    }
}

object ProductApp extends App {
  import ProductInterpreter._
  import ProductInterpreter.language._

  val product = pair(1, "hello")
  val fproduct: ProductReader[Int] = fst(product)
  val sproduct: ProductReader[String] = snd(product)

  val cea = fproduct.run(encoded)
  val ceb = sproduct.run(encoded)

  val ta = fproduct.run(tuple)
  val tb = sproduct.run(tuple)

  val s =
    s"""
    cea = ${cea}
    ceb = ${ceb}

    ta = ${ta}
    tb = ${tb}
    """

  println(s)
}
