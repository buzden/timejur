package ru.buzden.typelevel

trait IndexingMonoid[T] extends IndexingMonoidZZ[T] with IndexingSemigroup[T]

trait IndexingMonoidZZ[T] extends IndexingSemigroupZZ[T] {
  type Empty <: X[T]
  def empty: Empty
}

object IndexingMonoid {
  object syntax {
    def empty[T](implicit im: IndexingMonoidZZ[T]): im.Empty = im.empty
  }
}
