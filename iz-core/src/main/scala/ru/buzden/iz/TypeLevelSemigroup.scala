package ru.buzden.iz

/** Type-level semigroup */
trait TypeLevelSemigroup[I] {
  type |+|[A <: I, B <: I] <: I
}

/** Type-level semigroup with an ability to emerge appropriate value */
trait TwoFacedSemigroup[I] extends TypeLevelSemigroup[I] {
  /** Type of the result of the combining operation.
    *
    * This type is three-holed: first two are for types of combination
    * operands and the third one is for the type of the combination
    * (i.e. for `A |+| B`).
    */
  type CombineR[_, _, _]

  def combine[A <: I, B <: I]: CombineR[A, B, A |+| B]
}

object TypeLevelSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[I, A <: I](val a: A) extends AnyVal {
      def |+|[B <: I](b: B)(implicit eis: TwoFacedSemigroup[I]): eis.CombineR[A, B, eis.|+|[A, B]] = eis.combine
    }
  }
}
