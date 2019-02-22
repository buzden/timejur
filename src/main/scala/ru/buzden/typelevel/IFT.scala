package ru.buzden.typelevel

import cats.{Contravariant, Functor}

/** Ersatz for implicit function type */
case class IFT[A, B](f: A => B) extends AnyVal {
  def apply(implicit a: A): B = f(a)
}

object IFT {
  implicit def iftCovariantFunctor[X]: Functor[IFT[X, ?]] = new Functor[IFT[X, ?]] {
    override def map[A, B](fa: IFT[X, A])(f: A => B): IFT[X, B] = IFT(fa.f `andThen` f)
  }

  implicit def iftContravariantFunctor[Y]: Contravariant[IFT[?, Y]] = new Contravariant[IFT[?, Y]] {
    override def contramap[A, B](fa: IFT[A, Y])(f: B => A): IFT[B, Y] = IFT(fa.f `compose` f)
  }
}
