package ru.buzden.typelevel

trait IndexingMonoid[I] extends IndexingSemigroup[I] {
  type Empty <: I
  def empty: Empty
}

object IndexingMonoid {
  object syntax {
    def empty[I](implicit im: IndexingMonoid[I]): im.Empty = im.empty
  }
}
