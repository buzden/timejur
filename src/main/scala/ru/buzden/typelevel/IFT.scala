package ru.buzden.typelevel

/** Ersatz for implicit function type */
case class IFT[A, B](f: A => B) extends AnyVal {
  def apply(implicit a: A): B = f(a)
}
