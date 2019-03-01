package ru.buzden.timejur.computation

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.LessEqual
import ru.buzden.iz._

// todo It seems that `MaxT` should be `<: T`, but it is not provided when
//      we write `IndexedArrowChoice[T, TypelevellyTimed[?, ?, T, ?]]`. Compiler bug or my misunderstanding?
final case class IzTimed[-A, +B, T, MaxT](f: A => (B, T Refined LessEqual[MaxT]))

object IzTimed {
  implicit def typelevellyTimedIndexedArrow[T](co: EmergingTLMonoid[T], ch: EmergingTLSemigroup[T]): IzArrowChoice[T, IzTimed[?, ?, T, ?]] = new IzArrowChoice[T, IzTimed[?, ?, T, ?]] {
    override val composer: EmergingTLMonoid[T] = co
    override val chooser: EmergingTLSemigroup[T] = ch

    import composer.Empty

    override type FirstR[I_AB, R] = R

    override def lift[A, B](f: A => B): LiftR[IzTimed[A, B, T, Empty]] = ???

    override def first[A, B, C, I_AB <: T](fa: IzTimed[A, B, T, I_AB]): IzTimed[(A, C), (B, C), T, I_AB] = ???

    override def compose[A, B, C, I_AB <: T, I_BC <: T](f: IzTimed[B, C, T, I_BC], g: IzTimed[A, B, T, I_AB]): ComposeR[I_AB, I_BC, IzTimed[A, C, T, I_AB |>>| I_BC]] = ???

    override def choose[A, B, C, D, I_AC <: T, I_BD <: T](f: IzTimed[A, C, T, I_AC])(g: IzTimed[B, D, T, I_BD]): ChooseR[I_AC, I_BD, IzTimed[A Either B, C Either D, T, I_AC |\| I_BD]] = ???
  }
}
