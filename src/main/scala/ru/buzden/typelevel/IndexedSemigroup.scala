package ru.buzden.typelevel

trait IndexedSemigroup[T] {
  type XT = T with Singleton
  type |+|[A <: XT, B <: XT] <: XT

  def combine[A <: XT, B <: XT](a: A, b: B): A |+| B
}
