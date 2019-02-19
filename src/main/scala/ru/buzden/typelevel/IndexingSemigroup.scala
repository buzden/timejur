package ru.buzden.typelevel

trait IndexingSemigroup[T] extends IndexingSemigroupZZ[T] {
  import ru.buzden.typelevel.IndexingSemigroup.NewTypeUnit
  type ZZ[A, B] = NewTypeUnit

  def combine[A <: X[T], B <: X[T]]: A |+| B

  final def combineZZ[A <: X[T], B <: X[T]](implicit zz: ZZ[A, B]): A |+| B = combine[A, B]
}

object IndexingSemigroup {
  case class NewTypeUnit(u: Unit) extends AnyVal
  implicit val newTypeUnit: NewTypeUnit = NewTypeUnit(())

  object syntax {
    implicit class IndexedSemigroupOps[T, A <: X[T]](val a: A) extends AnyVal {
      def |+|[B <: X[T]](b: B)(implicit is: IndexingSemigroupZZ[T]) = new CombineCreation[T, A, B]

      def |+|[B <: X[T]](b: B)(implicit is: IndexingSemigroup[T]): is.|+|[A, B] = is.combine[A, B]
    }

    // Workaround of lack of multiple implicit arguments lists
    class CombineCreation[T, A <: X[T], B <: X[T]](implicit val is: IndexingSemigroupZZ[T]) {
      import is._
      def apply(implicit zz: is.ZZ[A, B]): A |+| B = is.combineZZ
    }
  }
}

trait IndexingSemigroupZZ[T] {
  // ZZ parameter is a cheating thing, a typelevel ability to require implicit of this type during combining.
  type ZZ[_, _]

  type |+|[A <: X[T], B <: X[T]] <: X[T]

  def combineZZ[A <: X[T], B <: X[T]](implicit zz: ZZ[A, B]): A |+| B
}
