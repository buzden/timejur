package ru.buzden.typelevel

trait IndexedMonoid[T] extends IndexedSemigroup[T] {
  type Empty <: XT

  def empty: Empty
}

object IndexedMonoid {
  object syntax {
    def empty[T](implicit im: IndexedMonoid[T]): im.Empty = im.empty
  }
}
