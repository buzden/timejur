package ru.buzden.typelevel

trait IndexingMonoid[I] extends IndexingSemigroup[I] {
  type Empty <: I
}

class SimpleIndexingMonoid[I, E <: I, C[A <: I, B <: I] <: I] extends SimpleIndexingSemigroup[I, C] with IndexingMonoid[I] {
  type Empty = E
}

trait EmergingIndexingMonoid[I] extends EmergingIndexingSemigroup[I] with IndexingMonoid[I] {
  /** Type of the result of the `empty` operation. */
  type EmptyR[_]

  def empty: EmptyR[Empty]
}

object IndexingMonoid {
  object syntax {
    def empty[I, A <: I, B <: I](implicit eim: EmergingIndexingMonoid[I]): eim.EmptyR[eim.Empty] = eim.empty
  }
}
