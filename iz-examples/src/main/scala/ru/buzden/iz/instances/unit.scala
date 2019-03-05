package ru.buzden.iz.instances

import ru.buzden.iz.{TwoFacedMonoid, TypeLevelCommutativeSemigroup}

object unit {
  implicit val unitHasIndexingMonoid: TwoFacedMonoid[Unit] = new TwoFacedMonoid[Unit] with TypeLevelCommutativeSemigroup[Unit] {
    override type Empty = Unit
    override type |+|[A, B] = Unit

    private implicit def unitIsFinal[A <: Unit]: Unit <:< A = implicitly[Unit <:< Unit].asInstanceOf[Unit <:< A]
    override def leftIdentityLaw[B <: Unit](b: B): Unit =:= B = <:<.antisymm
    override def rightIdentityLaw[A <: Unit](a: A): Unit =:= A = leftIdentityLaw[A](a)
    override def associativityLaw[A <: Unit, B <: Unit, C <: Unit](a: A, b: B, c: C): Unit =:= Unit = implicitly
    override def commutativityLaw[A <: Unit, B <: Unit](a: A, b: B): Unit =:= Unit = implicitly

    override def empty: Empty = ()
    override def combine[A, B](a: A, b: B): A |+| B = ()
  }
}
