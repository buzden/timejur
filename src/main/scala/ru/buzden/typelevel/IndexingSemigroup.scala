package ru.buzden.typelevel

// ZZ parameter is a cheating thing, a typelevel ability to require implicit of this type during combining.
trait IndexingSemigroup[T, ZZ[_, _]] {
  type |+|[A <: X[T], B <: X[T]] <: X[T]

  def combine[A <: X[T], B <: X[T]](implicit i: ZZ[A, B]): A |+| B
}

object IndexingSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[T, A <: X[T]](val a: A) extends AnyVal {
      def |+|[B <: X[T], ZZ[_, _]](b: B)(implicit i: ZZ[A, B], is: IndexingSemigroup[T, ZZ]): is.|+|[A, B] = is.combine
    }
  }
}
