package ru.buzden.iz

/** Type-level monoid */
trait TypeLevelMonoid[I] extends TypeLevelSemigroup[I] {
  type Empty <: I
}

/** Type-level monoid with an ability to emerge appropriate value */
trait TwoFacedMonoid[I] extends TwoFacedSemigroup[I] with TypeLevelMonoid[I] {
  /** Type of the result of the `empty` operation. */
  type EmptyR[_]

  def empty: EmptyR[Empty]
}

object TypeLevelMonoid {
  object syntax {
    def empty[I, A <: I, B <: I](implicit eim: TwoFacedMonoid[I]): eim.EmptyR[eim.Empty] = eim.empty
  }
}
