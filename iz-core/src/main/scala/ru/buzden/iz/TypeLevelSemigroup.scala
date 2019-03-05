package ru.buzden.iz

/** Type-level semigroup */
trait TypeLevelSemigroup[I] {
  type |+|[A <: I, B <: I] <: I
}

/** Type-level semigroup with an ability to emerge appropriate value */
trait TwoFacedSemigroup[I] extends TypeLevelSemigroup[I] {
  def combine[A <: I, B <: I](a: A, b: B): A |+| B
}

object TypeLevelSemigroup {
  object syntax {
    implicit class IzSemigroupOps[I, A <: I](val a: A) extends AnyVal {
      def |+|[B <: I](b: B)(implicit eis: TwoFacedSemigroup[I]): eis.|+|[A, B] = eis.combine(a, b)
    }
  }
}
