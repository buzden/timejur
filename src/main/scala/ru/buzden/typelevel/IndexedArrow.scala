package ru.buzden.typelevel

trait IndexedArrow[I, F[_, _, _]] {
  val im: IndexingMonoidZZ[I]
  import im._

  def lift[A, B](f: A => B): F[A, B, Empty]

  def first[A, B, C, I_AB](fa: F[A, B, I_AB]): F[(A, C), (B, C), I_AB]

  def compose[A, B, C, I_AB, I_BC](f: F[B, C, I_BC], g: F[A, B, I_AB]): F[A, C, I_AB |+| I_BC]
}
