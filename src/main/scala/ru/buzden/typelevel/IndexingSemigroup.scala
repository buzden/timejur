package ru.buzden.typelevel

trait IndexingSemigroup[T] {
  // ZZ parameter is a cheating thing, a typelevel ability to require implicit of this type during combining.
  type ZZ[_, _]

  type |+|[A <: X[T], B <: X[T]] <: X[T]

  def combine[A <: X[T], B <: X[T]](implicit zz: ZZ[A, B]): A |+| B
}

object IndexingSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[T, A <: X[T]](val a: A) extends AnyVal {
      def |+|[B <: X[T]](b: B)(implicit is: IndexingSemigroup[T]) = new CombineCreation[T, A, B]
    }

    // Workaround of lack of multiple implicit arguments lists
    class CombineCreation[T, A <: X[T], B <: X[T]](implicit val is: IndexingSemigroup[T]) {
      import is._
      def apply(implicit zz: is.ZZ[A, B]): A |+| B = is.combine
    }
  }
}
