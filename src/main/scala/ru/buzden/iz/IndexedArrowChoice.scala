package ru.buzden.iz

trait IndexedArrowChoice[I, F[_, _, _ <: I]] extends IndexedArrow[I, F] {
  val chooser: IndexingSemigroup[I]
  type |\|[A <: I, B <: I] = chooser.|+|[A, B]

  type ChooseR[I_AC, I_BD, R]

  def choose[A, B, C, D, I_AC <: I, I_BD <: I](f: F[A, C, I_AC])(g: F[B, D, I_BD]): ChooseR[I_AC, I_BD, F[A Either B, C Either D, I_AC |\| I_BD]]
}
