package tfm.examples

import cats.FlatMap
import cats.state.State
import cats.std.function._
import cats.syntax.flatMap._
import cats.syntax.functor._

import tfm.{fin, local}

@fin("TerminalIO")
trait TerminalInterpreter[F[_]] {
  @local def F: FlatMap[F]
  def join[A](lhs: F[A], rhs: F[A]): F[A]

  val readLine: F[String]
  def writeLine(string: String): F[Unit]
}

object TerminalInterpreter {
  val io: TerminalInterpreter[IO] =
    new TerminalInterpreter[IO] {
      val F: FlatMap[IO] = FlatMap[IO]
      def join[A](lhs: IO[A], rhs: IO[A]): IO[A] = lhs.flatMap(_ => rhs.map(identity))
      val readLine: IO[String] = IO { Console.readLine() }
      def writeLine(string: String): IO[Unit] = IO { println(string) }
    }

  type MockState[A] = State[Mock, A]
  val mock: TerminalInterpreter[MockState] =
    new TerminalInterpreter[MockState] {
      val F: FlatMap[MockState] = FlatMap[MockState]
      def join[A](lhs: MockState[A], rhs: MockState[A]): MockState[A] = lhs.flatMap(_ => rhs.map(identity))
      val readLine: MockState[String] = State(Mock.read)
      def writeLine(string: String): MockState[Unit] = State.modify(Mock.write(string))
    }

  implicit val terminalIOFlatMap: FlatMap[TerminalIO] =
    new FlatMap[TerminalIO] {
      def flatMap[A, B](fa: TerminalIO[A])(f: A => TerminalIO[B]): TerminalIO[B] =
        new TerminalIO[B] {
          def run[F[_]](interpreter: TerminalInterpreter[F]): F[B] =
            interpreter.F.flatMap(fa.run(interpreter))(a => f(a).run(interpreter))
        }

      def map[A, B](fa: TerminalIO[A])(f: A => B): TerminalIO[B] =
        new TerminalIO[B] {
          def run[F[_]](interpreter: TerminalInterpreter[F]): F[B] =
            interpreter.F.map(fa.run(interpreter))(f)
        }
    }
}

object TerminalIOApp extends App {
  import TerminalInterpreter.language._

  val program =
    for {
      _ <- writeLine("Enter something")
      x <- readLine
      _ <- writeLine("Enter another thing")
      y <- readLine
      _ <- writeLine(x ++ " " ++ y)
    } yield ()

  // Mock
  val init = Mock(in = List("Hello", "World"), out = List())
  println("Mock: " ++ program.run(TerminalInterpreter.mock).runS(init).run.toString)

  val program2 = join(writeLine("join1"), writeLine("join2"))
  val init2 = Mock(List(), List())
  println("Join: " ++ program2.run(TerminalInterpreter.mock).runS(init2).run.toString)

  // Real
  program.run(TerminalInterpreter.io).unsafePerformIO()
}





trait IO[A] { def unsafePerformIO(): A }

object IO {
  def apply[A](a: => A): IO[A] = new IO[A] { def unsafePerformIO(): A = a }

  implicit val ioMonad: FlatMap[IO] =
    new FlatMap[IO] {
      def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
        f(fa.unsafePerformIO())

      def map[A, B](fa: IO[A])(f: A => B): IO[B] =
        new IO[B] { def unsafePerformIO(): B = f(fa.unsafePerformIO()) }
    }
}

case class Mock(in: List[String], out: List[String])

object Mock {
  def read(mock: Mock): (Mock, String) = mock.in match {
    case Nil => (mock, "")
    case h :: t => (mock.copy(in = t), h)
  }

  def write(value: String)(mock: Mock): Mock =
    mock.copy(out = value :: mock.out)
}
