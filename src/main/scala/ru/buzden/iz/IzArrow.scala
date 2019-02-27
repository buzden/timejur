package ru.buzden.iz

trait IzArrow[I, F[_, _, _ <: I]] {
  val composer: IndexingMonoid[I]
  import composer.Empty
  type |>>|[A <: I, B <: I] = composer.|+|[A, B]

  type LiftR[R]
  type FirstR[I_AB, R]
  type ComposeR[I_AB, I_BC, R]

  def lift[A, B](f: A => B): LiftR[F[A, B, Empty]]

  def first[A, B, C, I_AB <: I](fa: F[A, B, I_AB]): FirstR[I_AB, F[(A, C), (B, C), I_AB]]

  def compose[A, B, C, I_AB <: I, I_BC <: I](f: F[B, C, I_BC], g: F[A, B, I_AB]): ComposeR[I_AB, I_BC, F[A, C, I_AB |>>| I_BC]]
}
