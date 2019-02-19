package ru.buzden.typelevel

trait IndexingSemigroup[T] {
  type |+|[A <: X[T], B <: X[T]] <: X[T]

  def combine[A <: X[T], B <: X[T]]: A |+| B
}

object IndexingSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[T, A <: X[T]](val a: A) extends AnyVal {
      def |+|[B <: X[T]](b: B)(implicit is: IndexingSemigroup[T]): is.|+|[A, B] = is.combine
    }
  }
}
