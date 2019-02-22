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
  val im: IndexingMonoid[I]
  import im._

  /** Type of the result of each operation */
  type R[_]
  // todo to think of individual result types per each operation

  def pure[A](a: A): R[F[A, Empty]]
  def flatMap[A, I_A <: I, B, I_B <: I](fa: F[A, I_A])(f: A => F[B, I_B]): R[F[B, I_A |+| I_B]]
}

object IndexedMonad {
  object syntax {
    implicit class IndexedMonadAnyAOps[A](val a: A) extends AnyVal {
      def pure[I, F[_, _]](implicit iM: IndexedMonad[I, F]): iM.R[F[A, iM.im.Empty]] = iM.pure(a)
    }

    implicit class IndexedMonadOps[I, F[_, _], A, I_A <: I](val fa: F[A, I_A]) extends AnyVal {
      def flatMap[B, I_B <: I](f: A => F[B, I_B])(implicit iM: IndexedMonad[I, F]): iM.R[F[B, iM.im.|+|[I_A, I_B]]] =
        iM.flatMap[A, I_A, B, I_B](fa)(f)
    }
  }
}
