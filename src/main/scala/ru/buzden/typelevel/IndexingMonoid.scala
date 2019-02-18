package ru.buzden.typelevel

trait IndexingMonoid[T] extends IndexingSemigroup[T] {
  type Empty <: X[T]

  def empty: Empty
}

object IndexingMonoid {
  object syntax {
    def empty[T](implicit im: IndexingMonoid[T]): im.Empty = im.empty
  }

  implicit val unitHasIndexingMonoid: IndexingMonoid[Unit] = new IndexingMonoid[Unit] {
    val u: X[Unit] = ().asInstanceOf[X[Unit]] // I don't know why I need to coerce here.

    override type Empty = X[Unit]
    override def empty: Empty = u

    override type |+|[A, B] = X[Unit]
    override def combine[A <: X[Unit], B <: X[Unit]](a: A, b: B): A |+| B = u
  }
}
