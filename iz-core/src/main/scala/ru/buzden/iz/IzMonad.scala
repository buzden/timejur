package ru.buzden.iz

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
trait IzMonad[I, F[_, _ <: I]] {
  /** Indexing monoid used to determine the resulting type of the monad operations.
    *
    * Dependency on the indexing monoid was done as a field by two reasons:
    * - to give an ability for `PureR` and `FlatMapR` types to depend on `EmptyR` and
    *   `CombinationR` of some emerging indexing monoid;
    * - to give an ability to implement monad instances specific to particular indexing monoid types.
    */
  val im: TypeLevelMonoid[I]
  import im._

  def pure[A](a: A): F[A, Empty]
  def flatMap[A, I_A <: I, B, I_B <: I](fa: F[A, I_A])(f: A => F[B, I_B]): F[B, I_A |+| I_B]
}

object IzMonad {
  object syntax {
    implicit class IzMonadAnyAOps[A](val a: A) extends AnyVal {
      def pure[I, F[_, _]](implicit iM: IzMonad[I, F]): F[A, iM.im.Empty] = iM.pure(a)
    }

    implicit class IzMonadOps[I, F[_, _], A, I_A <: I](val fa: F[A, I_A]) extends AnyVal {
      def flatMap[B, I_B <: I](f: A => F[B, I_B])(implicit iM: IzMonad[I, F]): F[B, iM.im.|+|[I_A, I_B]] =
        iM.flatMap[A, I_A, B, I_B](fa)(f)
    }
  }
}
