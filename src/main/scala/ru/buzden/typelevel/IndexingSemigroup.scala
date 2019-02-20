package ru.buzden.typelevel

trait IndexingSemigroup[I] extends IndexingSemigroupZZ[I] {
  import ru.buzden.typelevel.IndexingSemigroup.NewTypeUnit
  type ZZ[A, B] = NewTypeUnit

  def combine[A <: I, B <: I]: A |+| B

  final def combineZZ[A <: I, B <: I](implicit zz: ZZ[A, B]): A |+| B = combine[A, B]
}

object IndexingSemigroup {
  case class NewTypeUnit(u: Unit) extends AnyVal
  implicit val newTypeUnit: NewTypeUnit = NewTypeUnit(())

  object syntax {
    implicit class IndexedSemigroupOps[I, A <: I](val a: A) extends AnyVal {
      def |+|[B <: I](b: B)(implicit is: IndexingSemigroupZZ[I]) = new CombineCreation[I, A, B]

      def |+|[B <: I](b: B)(implicit is: IndexingSemigroup[I]): is.|+|[A, B] = is.combine[A, B]
    }

    // Workaround of lack of multiple implicit arguments lists
    class CombineCreation[I, A <: I, B <: I](implicit val is: IndexingSemigroupZZ[I]) {
      import is._
      def apply(implicit zz: is.ZZ[A, B]): A |+| B = is.combineZZ
    }
  }
}

trait IndexingSemigroupZZ[I] {
  // ZZ parameter is a cheating thing, a typelevel ability to require implicit of this type during combining.
  type ZZ[_, _]

  type |+|[A <: I, B <: I] <: I

  def combineZZ[A <: I, B <: I](implicit zz: ZZ[A, B]): A |+| B
}
