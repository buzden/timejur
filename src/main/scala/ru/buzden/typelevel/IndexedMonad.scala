package ru.buzden.typelevel

trait IndexedMonad[F[_, _]] {
  def pure[I, A](a: A)(implicit im: IndexingMonoid[I]): F[A, im.Empty]

  def flatMap[I, A, I_A <: I with Singleton, B, I_B <: I with Singleton]
    (fa: F[A, I_A])(f: A => F[B, I_B])(implicit is: IndexingSemigroup[I]): F[B, is.|+|[I_A, I_B]]
}
