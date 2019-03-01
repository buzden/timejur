package ru.buzden.iz

import cats.arrow.ArrowChoice
import cats.instances.function._
import cats.syntax.arrowChoice._
import cats.syntax.compose._
import cats.syntax.strong._
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

  implicit def iftArrowChoice: ArrowChoice[IFT] = new ArrowChoice[IFT] {
    override def lift[A, B](f: A => B): IFT[A, B] = IFT(f)
    override def compose[A, B, C](f: IFT[B, C], g: IFT[A, B]): IFT[A, C] = IFT(f.f <<< g.f)
    override def first[A, B, C](fa: IFT[A, B]): IFT[(A, C), (B, C)] = IFT(fa.f.first)
    override def choose[A, B, C, D](f: IFT[A, C])(g: IFT[B, D]): IFT[Either[A, B], Either[C, D]] = IFT(f.f +++ g.f)
  }
}
