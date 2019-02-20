package ru.buzden.typelevel

trait IndexingSemigroup[I] {
  type |+|[A <: I, B <: I] <: I
}

class SimpleIndexingSemigroup[I, C[A <: I, B <: I] <: I] extends IndexingSemigroup[I] {
  type |+|[A <: I, B <: I] = C[A, B]
}

trait IndexingSemigroupEmerger[I, A <: I, B <: I] {
  val proto: IndexingSemigroup[I]
  import proto.|+|

  def combine: A |+| B
}

object IndexingSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[I, A <: I](val a: A) extends AnyVal {
      def |+|[B <: I](b: B)(implicit ise: IndexingSemigroupEmerger[I, A, B]): ise.proto.|+|[A, B] = ise.combine
    }
  }
}
