package ru.buzden.iz

/** Type-level semigroup */
trait TLSemigroup[I] {
  type |+|[A <: I, B <: I] <: I
}

/** Type-level semigroup with an ability to emerge appropriate value */
trait EmergingTLSemigroup[I] extends TLSemigroup[I] {
  /** Type of the result of the combining operation.
    *
    * This type is three-holed: first two are for types of combination
    * operands and the third one is for the type of the combination
    * (i.e. for `A |+| B`).
    */
  type CombinationR[_, _, _]

  def combine[A <: I, B <: I]: CombinationR[A, B, A |+| B]
}

object TLSemigroup {
  object syntax {
    implicit class IndexedSemigroupOps[I, A <: I](val a: A) extends AnyVal {
      def |+|[B <: I](b: B)(implicit eis: EmergingTLSemigroup[I]): eis.CombinationR[A, B, eis.|+|[A, B]] = eis.combine
    }
  }
}
