package ru.buzden.typelevel

trait IndexedMonoid[T] extends IndexedSemigroup[T] {
  type Empty <: XT

  def empty: Empty
}
