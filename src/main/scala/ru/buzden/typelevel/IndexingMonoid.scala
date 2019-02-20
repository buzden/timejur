package ru.buzden.typelevel

trait IndexingMonoid[I] extends IndexingMonoidZZ[I] with IndexingSemigroup[I]

trait IndexingMonoidZZ[I] extends IndexingSemigroupZZ[I] {
  type Empty <: I
  def empty: Empty
}

object IndexingMonoid {
  object syntax {
    def empty[I](implicit im: IndexingMonoidZZ[I]): im.Empty = im.empty
  }
}
