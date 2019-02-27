package ru.buzden.iz

trait IndexingMonoid[I] extends IndexingSemigroup[I] {
  type Empty <: I
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
