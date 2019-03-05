package ru.buzden.iz.instances

import ru.buzden.iz.{TwoFacedMonoid, TypeLevelCommutativeSemigroup}

object unit {
  implicit val unitHasIndexingMonoid: TwoFacedMonoid[Unit] = new TwoFacedMonoid[Unit] with TypeLevelCommutativeSemigroup[Unit] {
    override type Empty = Unit
    override type |+|[A, B] = Unit

    private implicit def unitIsFinal[A <: Unit]: Unit <:< A = implicitly[Unit <:< Unit].asInstanceOf[Unit <:< A]
    override def leftIdentityLaw[B <: Unit]: Unit =:= B = <:<.antisymm
    override def rightIdentityLaw[A <: Unit]: Unit =:= A = leftIdentityLaw[A]
    override def associativityLaw[A <: Unit, B <: Unit, C <: Unit]: Unit =:= Unit = implicitly
    override def commutativityLaw[A <: Unit, B <: Unit]: Unit =:= Unit = implicitly

    override def empty: Empty = ()
    override def combine[A, B](a: A, b: B): A |+| B = ()
  }
}
