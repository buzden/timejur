package ru.buzden.typelevel

trait IndexingSemigroup[I] {
  type |+|[A <: I, B <: I] <: I
}

trait IndexingSemigroupCombiner[I, A <: I, B <: I] {
  val is: IndexingSemigroup[I]
  import is.|+|

  def combine: A |+| B
}

object IndexingSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[I, A <: I](val a: A) extends AnyVal {
      def |+|[B <: I](b: B)(implicit isc: IndexingSemigroupCombiner[I, A, B]): isc.is.|+|[A, B] = isc.combine
    }
  }
}
