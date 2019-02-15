package ru.buzden.typelevel

trait Addable[T] {
  type XT = T with Singleton
  type +[A <: XT, B <: XT] <: XT

  def add[A <: XT, B <: XT](a: A, b: B): A + B
}
