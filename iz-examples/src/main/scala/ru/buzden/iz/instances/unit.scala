package ru.buzden.iz.instances

import ru.buzden.iz.TwoFacedMonoid

object unit {
  implicit val unitHasIndexingMonoid: TwoFacedMonoid[Unit] = new TwoFacedMonoid[Unit] {
    override type EmptyR[A] = A
    override type CombineR[A, B, C] = C

    override type Empty = Unit
    override type |+|[A, B] = Unit

    override def empty: Empty = ()
    override def combine[A, B](a: A, b: B): A |+| B = ()
  }
}
