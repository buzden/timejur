package ru.buzden.typelevel

/**
  * Typeclass for the indexed monad with singleton index types.
  *
  * Described indexed monad type is two-holed:
  * - type of value (analogue of usual monad's type parameter);
  * - singleton type of particular index value (which changes during operations).
  *
  * The third type argument must be a singleton and must
  * inherit the second type argument at the same time.
  *
  * @tparam I used general type of index
  * @tparam F described three-holed monad type
  */
trait IndexedMonad[I, F[_, _ <: I]] {
  /** Indexing monoid used to determine the resulting type of the monad operations.
    *
    * Dependency on the indexing monoid was done as a field by two reasons:
    * - to give an ability for `PureR` and `FlatMapR` types to depend on `EmptyR` and
    *   `CombinationR` of some emerging indexing monoid;
    * - to give an ability to implement monad instances specific to particular indexing monoid types.
    */
  val im: IndexingMonoid[I]
  import im._

  /** Type of the result of `pure` operation */
  type PureR[_]
  /** Type of the result of `flatMap` operation
    *
    * The first two type holes are for index arguments of monad values for types `A` and `B` accordingly.
    * The last type hole is for the result type.
    */
  type FlatMapR[I_A, I_B, _]

  def pure[A](a: A): PureR[F[A, Empty]]
  def flatMap[A, I_A <: I, B, I_B <: I](fa: F[A, I_A])(f: A => F[B, I_B]): FlatMapR[A, B, F[B, I_A |+| I_B]]
}

abstract class SimpleIndexedMonad[I: IndexingMonoid, F[_, _ <: I]] extends IndexedMonad[I, F] {
  override val im: IndexingMonoid[I] = implicitly

  override type PureR[A] = A
  override type FlatMapR[A, B, C] = C
}

abstract class OrdinaryIndexedMonad[I: EmergingIndexingMonoid, F[_, _ <: I]] extends IndexedMonad[I, F] {
  override val im: EmergingIndexingMonoid[I] = implicitly

  override type PureR[A] = im.EmptyR[A]
  override type FlatMapR[A, B, C] = im.CombinationR[A, B, C]
}

object IndexedMonad {
  object syntax {
    implicit class IndexedMonadAnyAOps[A](val a: A) extends AnyVal {
      def pure[I, F[_, _]](implicit iM: IndexedMonad[I, F]): iM.PureR[F[A, iM.im.Empty]] = iM.pure(a)
    }

    implicit class IndexedMonadOps[I, F[_, _], A, I_A <: I](val fa: F[A, I_A]) extends AnyVal {
      def flatMap[B, I_B <: I](f: A => F[B, I_B])(implicit iM: IndexedMonad[I, F]): iM.FlatMapR[A, B, F[B, iM.im.|+|[I_A, I_B]]] =
        iM.flatMap[A, I_A, B, I_B](fa)(f)
    }
  }
}
