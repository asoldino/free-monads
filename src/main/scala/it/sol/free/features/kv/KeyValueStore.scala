package it.sol.free.features.kv

import cats.free.{Free, Inject}

trait KeyValueStore[K, V] {
  def put(key: K, value: V): Unit
  def get(key: K): V
}

object KeyValueStore {
  /**
    * Algebra for key-value store
    * @tparam A Return type for commands
    * @tparam K key-value key type
    * @tparam V key-value value type
    */
  sealed trait KVSAlgebraCommand[A, K, V] extends Product with Serializable
  private[kv] case class Put[K, V](key: K, value: V) extends KVSAlgebraCommand[Unit, K, V]
  private[kv] case class Get[K, V](key: K) extends KVSAlgebraCommand[Option[String], K, V]

  /**
    * Represents String to String map type
    * @tparam A Return type for commands that follows this algebra
    *
    * Additional key-value combinations may go here
    */
  type StringToStringKV[A] = KVSAlgebraCommand[A, String, String]
}

import KeyValueStore._

/**
  * key-value store api
  * @param I Implicit injector for monad container
  * @tparam F algebra descriptor (trait)
  */
class KVSAlgebra[F[_]](implicit I: Inject[StringToStringKV, F]) {
  /**
    * Issues a "put" command returning a lazy evaluated [[Put]] to the interpreter.
    *
    * The semantic of pull is to create (or update if already existent) the provided key with the provided value
    *
    * @param key key
    * @param value value
    * @return Free[F, Unit] Monad, lazily evaluated (see [[Free.inject]])
    */
  def put(key: String, value: String): Free[F, Unit] = Free.inject[StringToStringKV, F](Put(key, value))

  /**
    * Issues a "put" command returning a lazy evaluated [[Get]] to the interpreter.
    *
    * The semantic of get is to return the value associated to the **key**
    *
    * @param key key
    * @return Free[F, String] Monad, lazily evaluated (see [[Free.inject]])
    */
  def get(key: String): Free[F, Option[String]] = Free.inject[StringToStringKV, F](Get(key))
}

/**
  * Contains the implicit creator for any registered monad F
  */
object KVSAlgebra {
  implicit def instance[F[_]](implicit I: Inject[StringToStringKV, F]) = new KVSAlgebra[F]
}

