package ru.buzden.typelevel

trait IndexingMonoid[T, Z[_, _]] extends IndexingSemigroup[T, Z] {
  type Empty <: X[T]

  def empty: Empty
}

object IndexingMonoid {
  object syntax {
    def empty[T, Z[_, _]](implicit im: IndexingMonoid[T, Z]): im.Empty = im.empty
  }
}
