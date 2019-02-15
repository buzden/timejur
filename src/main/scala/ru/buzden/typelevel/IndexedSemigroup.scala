package ru.buzden.typelevel

trait IndexedSemigroup[T] {
  type XT = T with Singleton
  type |+|[A <: XT, B <: XT] <: XT

  def combine[A <: XT, B <: XT](a: A, b: B): A |+| B
}

object IndexedSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[T, A <: T with Singleton](val a: A) extends AnyVal {
      def |+|[B <: T with Singleton](b: B)(implicit is: IndexedSemigroup[T]): is.|+|[A, B] = is.combine(a, b)
    }
  }
}
