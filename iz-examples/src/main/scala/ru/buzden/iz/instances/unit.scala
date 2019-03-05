package ru.buzden.iz.instances

import ru.buzden.iz.{TwoFacedMonoid, TypeLevelCommutativeSemigroup}

object unit {
  implicit val unitHasIndexingMonoid: TwoFacedMonoid[Unit] = new TwoFacedMonoid[Unit] with TypeLevelCommutativeSemigroup[Unit] {
    override type Empty = Unit
    override type |+|[A, B] = Unit

    override def empty: Empty = ()
    override def combine[A, B](a: A, b: B): A |+| B = ()

    override def leftIdentityLaw[B <: Unit]: Unit =:= B = ???
    override def rightIdentityLaw[A <: Unit]: Unit =:= A = ???
    override def associativityLaw[A <: Unit, B <: Unit, C <: Unit]: Unit =:= Unit = implicitly
    override def commutativityLaw[A <: Unit, B <: Unit]: Unit =:= Unit = implicitly
  }
}
