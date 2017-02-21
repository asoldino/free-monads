package it.sol.free.features.kv

import cats.free.Free
import cats.~>
import it.sol.free.features.kv.KeyValueStore.StringToStringKV

import scala.concurrent.{Await, Future}
import scala.util.Random

/**
  * Facade API
  */
object KVApi {
  /**
    * Defines the set of active and imported algebras
    * @tparam A instruction response, can be anything
    */
  type Application[A] = StringToStringKV[A]

  def storeRandomStuffAndReturnIt(implicit A: KVSAlgebra[Application]): Free[Application, Option[String]] = {
    import A._

    val key = Random.nextString(10)
    for {
      _   <- put(key, Random.nextString(20))
      ret <- get(key + "asd")
    } yield ret
  }
}


object KVMain extends App {
  import KVSAlgebra._
  import it.sol.free.features.Utils._

  import scala.concurrent.duration._

  val interpreter: StringToStringKV ~> Future = new InMemoryAlgebraInterpreterWithFuture

  val result = Await.result(KVApi.storeRandomStuffAndReturnIt foldMap interpreter, 5 seconds)

  println(result)
}