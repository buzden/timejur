package ru.buzden.iz

/** Type-level monoid */
//noinspection ScalaUnnecessaryParentheses
trait TypeLevelMonoid[I] extends TypeLevelSemigroup[I] {
  type Empty <: I

  def leftIdentityLaw[B <: I](b: B): (Empty |+| B) =:= B
  def rightIdentityLaw[A <: I](a: A): (A |+| Empty) =:= A
}

/** Type-level monoid with an ability to emerge appropriate value */
trait TwoFacedMonoid[I] extends TwoFacedSemigroup[I] with TypeLevelMonoid[I] {
  def empty: Empty
}

object TypeLevelMonoid {
  object syntax {
    def empty[I, A <: I, B <: I](implicit eim: TwoFacedMonoid[I]): eim.Empty = eim.empty
  }
}
