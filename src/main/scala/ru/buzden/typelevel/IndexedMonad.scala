package ru.buzden.typelevel

trait IndexedMonad[F[_, _]] {
  def pure[I, A](a: A)(implicit im: IndexingMonoid[I]): F[A, im.Empty]

  def flatMap[I, A, I_A <: I with Singleton, B, I_B <: I with Singleton]
    (fa: F[A, I_A])(f: A => F[B, I_B])(implicit is: IndexingSemigroup[I]): F[B, is.|+|[I_A, I_B]]
}

object IndexedMonad {
  object syntax {
    implicit class IndexedMonadAnyAOps[A](val a: A) extends AnyVal {
      def pure[F[_, _], I](implicit iM: IndexedMonad[F], im: IndexingMonoid[I]): F[A, im.Empty] = iM.pure(a)(im)
    }

    implicit class IndexedMonadOps[F[_, _], A, I, I_A <: I with Singleton](val fa: F[A, I_A]) extends AnyVal {
      def flatMap[B, I_B <: I with Singleton]
          (f: A => F[B, I_B])(implicit iM: IndexedMonad[F], is: IndexingSemigroup[I]): F[B, is.|+|[I_A, I_B]] =
        iM.flatMap[I, A, I_A, B, I_B](fa)(f)(is)
    }
  }
}
