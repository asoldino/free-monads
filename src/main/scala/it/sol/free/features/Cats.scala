package it.sol.free.features

// AST
sealed trait Interaction[A] extends Product with Serializable
case class Ask(prompt: String) extends Interaction[String]
case class Tell(message: String) extends Interaction[Unit]

sealed trait DataOp[A] extends Product with Serializable
case class AddItem(name: String) extends DataOp[Unit]
case class ValidateItem(item: String) extends DataOp[Boolean]
case class GetAllItems() extends DataOp[List[String]]

sealed trait SomethingElse[A] extends Product with Serializable
case class DoSomethingElse() extends SomethingElse[Unit]

// Free and lifting
import cats.free.{Free, Inject}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.{Failure, Left, Right, Success}
import scalaz.\/-

class Interactions[F[_]](implicit I: Inject[Interaction, F]) {
  def ask(prompt: String): Free[F, String] = Free.inject[Interaction, F](Ask(prompt))

  def tell(message: String): Free[F, Unit] = Free.inject[Interaction, F](Tell(message))
}

object Interactions {
  implicit def instance[F[_]](implicit I: Inject[Interaction, F]) = new Interactions[F]
}

class DataSource[F[_]](implicit I: Inject[DataOp, F]) {
  def addItem(name: String): Free[F, Unit] = Free.inject[DataOp, F](AddItem(name))

  def validateItem(item: String): Free[F, Boolean] = Free.inject[DataOp, F](ValidateItem(item))

  def getAllItems: Free[F, List[String]] = Free.inject[DataOp, F](GetAllItems())
}

object DataSource {
  implicit def instance[F[_]](implicit I: Inject[DataOp, F]) = new DataSource[F]
}

class ThirdLifting[F[_]](implicit I: Inject[SomethingElse, F]) {
  def doSomethingElse: Free[F, Unit] = Free.inject[SomethingElse, F](DoSomethingElse())
}

object ThirdLifting {
  implicit def instance[F[_]](implicit I: Inject[SomethingElse, F]) = new ThirdLifting[F]
}

// Coproduct
import cats.data.Coproduct
import cats.~>

import scalaz.concurrent.Task

object ConsoleInteractions extends (Interaction ~> Task) {
  override def apply[A](fa: Interaction[A]) = fa match {
    case Ask(prompt) => Task.delay {
      scala.io.StdIn.readLine(prompt)
    }

    case Tell(message) => Task.delay {
      println(message)
    }
  }
}

object InMemoryDataSource extends (DataOp ~> Task) {
  private[this] val dataSet: ListBuffer[String] = ListBuffer()

  override def apply[A](fa: DataOp[A]): Task[A] = fa match {
    case AddItem(name) => Task.delay {
      dataSet.append(name)
    }
    case ValidateItem(item) => Task.now {
      item.length > 2
    }
    case GetAllItems() => Task.delay {
      dataSet.toList
    }
  }
}

object SomethingElseWithConsoleAppenderAgain extends (SomethingElse ~> Task) {
  override def apply[A](fa: SomethingElse[A]): Task[A] = fa match {
    case DoSomethingElse() => Task.now {
      println("Yay something else can be done")
    }
  }
}

// Main
object CatsMain extends App {
  type PartialApi[A] = Coproduct[DataOp, SomethingElse, A]
  type Application[A] = Coproduct[Interaction, PartialApi, A]

  val partialApiInterpreter: PartialApi ~> Task = InMemoryDataSource or SomethingElseWithConsoleAppenderAgain
  val interpreter: Application ~> Task = ConsoleInteractions or partialApiInterpreter

  import cats.Monad

  implicit val taskMonad = new Monad[Task] {
    override def flatMap[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] = fa flatMap f

    @tailrec override def tailRecM[A, B](a: A)(f: (A) => Task[Either[A, B]]): Task[B] = f(a).unsafePerformSync match {
      case Left(value) => tailRecM(value)(f)
      case Right(value) => Task.now(value)
    }

    override def pure[A](x: A): Task[A] = Task.now(x)
  }

  def program(implicit I: Interactions[Application], D: DataSource[Application], S: ThirdLifting[Application]): Free[Application, List[String]] = {
    import D._
    import I._
    import S._

    for {
      name <- ask("Insert object name")
      _     <- doSomethingElse
      valid <- validateItem(name)
      _ <- if (valid) addItem(name) else tell(s"$name is not valid")
      allItems <- getAllItems
      _ <- tell(allItems.mkString("\n"))
    } yield allItems
  }

  val result = (program foldMap interpreter).unsafePerformSyncAttempt.getOrElse(println("Error " + _))

  println(result)
}