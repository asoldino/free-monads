package it.sol.free.features.kv

import cats.~>
import it.sol.free.features.kv.KeyValueStore.{Get, Put, StringToStringKV}

import scala.concurrent.Future

/**
  * Example implementation of a KVSAlgebra interpreter.
  * This interpreter is an arrow function implementation from KVSAlgebra command => Future
  *
  *   + Single point for the [[Future]] [[scala.concurrent.ExecutionContext]] requirement resolution
  *
  *   - No much help from the type system, the futures can return just anything (A)
  *
  * @param startingBackend
  */
class InMemoryAlgebraInterpreterWithFuture(startingBackend: Map[String, String] = Map()) extends (StringToStringKV ~> Future) {
  import scala.concurrent.ExecutionContext.Implicits.global

  var backend: Map[String, String] = startingBackend

  override def apply[A](fa: StringToStringKV[A]): Future[A] = fa match {
    case Put(key: String, value: String) => Future {
      backend = backend.updated(key, value)
      ().asInstanceOf[A]
    }
    case Get(key: String) => {
      Future.successful {
        backend.get(key).asInstanceOf[A]
      }
    }
  }
}