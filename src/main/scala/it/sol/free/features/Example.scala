package it.sol.free.features

import scalaz.{Free, Functor}

sealed trait Example[Next] extends Product with Serializable

case class Add[Next](next: Next) extends Example[Next]

case class Subtract[Next](next: Next) extends Example[Next]

case class Set[Next](value: Int, next: Next) extends Example[Next]

case class Get[Next](onValue: Int => Next) extends Example[Next]

object Example {
  implicit val exampleFunctor: Functor[Example] = new Functor[Example] {
    override def map[A, B](fa: Example[A])(f: (A) => B): Example[B] = fa match {
      case Add(next)        => Add(f(next))
      case Subtract(next)   => Subtract(f(next))
      case Set(value, next) => Set(value, f(next))
      case Get(onValue)     => Get(onValue andThen f)
    }
  }

  import Free.liftF

  def add: Free[Example, Unit] = liftF(Add(()))
  def subtract: Free[Example, Unit] = liftF(Subtract(()))
  def set(value: Int): Free[Example, Unit] = liftF(Set(value, ()))
  def get: Free[Example, Int] = liftF(Get(identity))
}

object Main extends App {
  import Example._

  val program = for {
    _ <- set(0)
    _ <- subtract
    _ <- subtract
    _ <- subtract
    _ <- subtract
    _ <- subtract
    v <- get
    _ <- set(42 + v)
  } yield ()

  def interpreterPure(program: Free[Example, Unit], value: Int = 0): Int = program.resume.fold({
    case Set(value, next) => interpreterPure(next, value)
    case Add(next)        => interpreterPure(next, value + 1)
    case Subtract(next)   => interpreterPure(next, value - 1)
    case Get(onValue)     => interpreterPure(onValue(value), value)
  }, _ => value)

  println(interpreterPure(program))
}