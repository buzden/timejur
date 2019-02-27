package ru.buzden.iz

trait TLMonoid[I] extends TLSemigroup[I] {
  type Empty <: I
}

trait EmergingTLMonoid[I] extends EmergingTLSemigroup[I] with TLMonoid[I] {
  /** Type of the result of the `empty` operation. */
  type EmptyR[_]

  def empty: EmptyR[Empty]
}

object TLMonoid {
  object syntax {
    def empty[I, A <: I, B <: I](implicit eim: EmergingTLMonoid[I]): eim.EmptyR[eim.Empty] = eim.empty
  }
}
