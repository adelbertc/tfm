package tfm

import org.specs2.Specification

class finTests extends Specification {
  import Algebras._

  def is =
    s2"""
    basic usage ${basicUsage}
    augments existing companion ${companion}
    allows abstract class interpreter ${abstractClass}
    supports implementation ${implementation}
    supports parametric methods ${parametric}
    supports higher-arity effects ${higherArity}
    supports effectful parameters ${effectful}
    supports non-effectful return type ${nonEffectful}
    supports named algebra ${namedAlgebra}
    supports named aux algebra ${namedAuxAlgebra}
    """

  def basicUsage = {
    import BasicInterp._
    import BasicInterp.language._

    val x = 1
    val program1: BasicOp[Int] = lit(x)
    val program2: BasicOp[String] = name
    (program1.run(id) mustEqual x) &&
    (program2.run(id) mustEqual s)
  }

  def companion = BasicInterp.id mustEqual BasicInterp.id

  def abstractClass = {
    import AbstractInterp._
    import AbstractInterp.language._

    val x = 5
    val program: AbstractOp[Int] = lit(x)
    program.run(id) mustEqual x
  }

  def implementation = {
    import ImplInterp._
    import ImplInterp.language._

    val x: ImplOp[Int] = foo(1, 2)
    val y: ImplOp[Int] = fooSwapped(1, 2)

    ok
  }

  def parametric = {
    import ParametricInterp._
    import ParametricInterp.language._

    val char: ParametricOp[Char] = lift('a')
    val int: ParametricOp[Int] = lift(1)
    val string: ParametricOp[String] = lift("hello")
    val vector: ParametricOp[Vector[Byte]] = lift(Vector.empty[Byte])

    ok
  }

  def higherArity = {
    import ArityInterp._
    val p: ArityOp[Int, String] = language.pair(1, "hello")
    ok
  }

  def effectful = {
    import EffectInterp._
    import EffectInterp.language._
    val program: EffectOp[Int] = add(lit(0), lit(1))
    ok
  }

  def nonEffectful = {
    import NonEffectInterp._
    import NonEffectInterp.language._
    val i = 5
    val program1: NonEffectReader[Int] = identity(i)
    val program2: NonEffectReader[String] = aString
    (program1.run(id) mustEqual i) &&
    (program2.run(id) mustEqual s)
  }

  def namedAlgebra = {
    import NamedAlgebraInterp._
    import NamedAlgebraInterp.language._
    val n = "named"
    val program1: NamedAlgebraOp[Int] = named(n)
    program1.run(id) mustEqual n.size
  }

  def namedAuxAlgebra = {
    import NamedAuxAlgebraInterp._
    import NamedAuxAlgebraInterp.language._
    val x = 5
    val program1: NamedAuxAlgebraOp2[Int] = noop(x)
    program1.run(id) mustEqual x
  }
}

object Algebras {
  type Id[A] = A

  @fin("BasicOp")
  trait BasicInterp[F[_]] {
    def lit(i: Int): F[Int]
    val name: F[String]
  }

  object BasicInterp {
    val s: String = "name"
    val id: BasicInterp[Id] =
      new BasicInterp[Id] {
        def lit(i: Int): Int = i
        val name: String = s
      }
  }

  @fin("AbstractOp")
  abstract class AbstractInterp[F[_]] {
    def lit(i: Int): F[Int]
  }

  object AbstractInterp {
    val id: AbstractInterp[Id] =
      new AbstractInterp[Id] {
        def lit(i: Int): Int = i
      }
  }

  @fin("ImplOp")
  trait ImplInterp[F[_]] {
    def foo(i: Int, j: Int): F[Int]
    def fooSwapped(i: Int, j: Int): F[Int] = fooSwapped(j, i)
  }

  @fin("ParametricOp")
  trait ParametricInterp[F[_]] {
    def lift[A](a: A): F[A]
  }

  @fin("ArityOp")
  trait ArityInterp[F[_, _]] {
    def pair[A, B](a: A, b: B): F[A, B]
  }

  @fin("EffectOp")
  trait EffectInterp[F[_]] {
    def lit(n: Int): F[Int]
    def add(lhs: F[Int], rhs: F[Int]): F[Int]
  }

  @fin("NonEffectOp", "NonEffectReader")
  trait NonEffectInterp[F[_]] {
    def identity[A](a: A): A
    def aString: String = NonEffectInterp.s
  }

  object NonEffectInterp {
    val s = "non-effect op"
    val id: NonEffectInterp[Id] =
      new NonEffectInterp[Id] {
        def identity[A](a: A): A = a
      }
  }

  @fin(algebraName = "NamedAlgebraOp")
  trait NamedAlgebraInterp[F[_]] {
    def named(s: String): F[Int]
  }

  object NamedAlgebraInterp {
    val id: NamedAlgebraInterp[Id] =
      new NamedAlgebraInterp[Id] {
        def named(s: String): Int = s.size
      }
  }

  @fin(algebraName = "NamedAuxAlgebraOp1", auxAlgebraName = "NamedAuxAlgebraOp2")
  trait NamedAuxAlgebraInterp[F[_]] {
    def noop(i: Int): Int
  }

  object NamedAuxAlgebraInterp {
    val id: NamedAuxAlgebraInterp[Id] =
      new NamedAuxAlgebraInterp[Id] {
        def noop(i: Int): Int = i
      }
  }
}
