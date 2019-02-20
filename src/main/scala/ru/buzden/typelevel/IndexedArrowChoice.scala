package ru.buzden.typelevel

trait IndexedArrowChoice[I, F[_, _, _ <: I]] extends IndexedArrow[I, F] {
  val chooser: IndexingMonoidZZ[I]
  import chooser._

  def choose[A, B, C, D, I_AC <: I, I_BD <: I](f: F[A, C, I_AC])(g: F[B, D, I_BD]): F[Either[A, B], Either[C, D], I_AC |+| I_BD]
}
