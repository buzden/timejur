package ru.buzden.typelevel

/**
  * Typeclass for the indexed monad with singleton index types.
  *
  * Described indexed monad type is three-holed:
  * - type of value (analogue of usual monad's type parameter);
  * - general type of index (which remains unchanged during operations);
  * - singleton type of particular index value (which changes during operations).
  *
  * The third type argument must be a singleton and must
  * inherit the second type argument at the same time.
  *
  * @tparam F described three-holed monad type
  */
trait IndexedMonad[F[_, I, _ <: I with Singleton]] {
  def pure[I, A](a: A)(implicit im: IndexingMonoid[I]): F[A, I, im.Empty]

  def flatMap[I, A, I_A <: I with Singleton, B, I_B <: I with Singleton]
    (fa: F[A, I, I_A])(f: A => F[B, I, I_B])(implicit is: IndexingSemigroup[I]): F[B, I, is.|+|[I_A, I_B]]
}

object IndexedMonad {
  object syntax {
    implicit class IndexedMonadAnyAOps[A](val a: A) extends AnyVal {
      def pure[F[_, _, _], I](implicit iM: IndexedMonad[F], im: IndexingMonoid[I]): F[A, I, im.Empty] =
        iM.pure(a)(im)
    }

    implicit class IndexedMonadOps[F[_, _, _], A, I, I_A <: I with Singleton](val fa: F[A, I, I_A]) extends AnyVal {
      def flatMap[B, I_B <: I with Singleton]
          (f: A => F[B, I, I_B])(implicit iM: IndexedMonad[F], is: IndexingSemigroup[I]): F[B, I, is.|+|[I_A, I_B]] =
        iM.flatMap[I, A, I_A, B, I_B](fa)(f)(is)
    }
  }
}
