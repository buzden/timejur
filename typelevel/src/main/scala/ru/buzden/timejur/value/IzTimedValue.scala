package ru.buzden.timejur.value

import ru.buzden.iz._

/** Timed value that contains time only at the type level */
final case class IzTimedValue[A, ATime](value: A)

object IzTimedValue {
  implicit def izMonadForIzTimedValue[T: TypeLevelMonoid]: IzMonad[T, IzTimedValue] = new IzMonad[T, IzTimedValue] {
    override val im: TypeLevelMonoid[T] = implicitly
    import im._

    override type PureR[A] = A
    override type FlatMapR[ATime, BTime, C] = C

    override def pure[A](a: A): IzTimedValue[A, Empty] =
      IzTimedValue[A, Empty](a)

    override def flatMap[A, ATime <: T, B, BTime <: T]
    (fa: IzTimedValue[A, ATime])(f: A => IzTimedValue[B, BTime]): IzTimedValue[B, ATime |+| BTime] =
      IzTimedValue[B, ATime |+| BTime](f(fa.value).value)
  }
}
