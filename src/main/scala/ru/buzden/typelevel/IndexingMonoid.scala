package ru.buzden.typelevel

trait IndexingMonoid[T] extends IndexingSemigroup[T] {
  type Empty <: XT

  def empty: Empty
}

object IndexingMonoid {
  object syntax {
    def empty[T](implicit im: IndexingMonoid[T]): im.Empty = im.empty
  }
}
