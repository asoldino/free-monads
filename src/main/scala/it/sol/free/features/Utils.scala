package it.sol.free.features

import cats.Monad

import scala.annotation.tailrec
import scala.concurrent.Future

object Utils {
  implicit val futureMonad: Monad[Future] = new Monad[Future] {
    import scala.concurrent._
    import duration._
    import ExecutionContext.Implicits.global


    override def pure[A](x: A) = Future.successful(x)

    override def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]) = fa flatMap f

    @tailrec override def tailRecM[A, B](a: A)(f: (A) => Future[Either[A, B]]): Future[B] = Await.result(f(a), 5 seconds) match {
      case Left(value)  => tailRecM(value)(f)
      case Right(value) => Future.successful(value)
    }
  }
}
