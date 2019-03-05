package ru.buzden.iz

trait IzArrowChoice[I, F[_, _, _ <: I]] extends IzArrow[I, F] {
  val chooser: TypeLevelSemigroup[I]
  type |\|[A <: I, B <: I] = chooser.|+|[A, B]

  def choose[A, B, C, D, I_AC <: I, I_BD <: I](f: F[A, C, I_AC])(g: F[B, D, I_BD]): F[A Either B, C Either D, I_AC |\| I_BD]
}
