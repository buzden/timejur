package ru.buzden.timejur.value

import ru.buzden.typelevel.IndexingSemigroup

final case class IndexedTimedValue[A, T, ATime <: T with Singleton](value: A) {
  def flatMap[B, BTime <: T with Singleton](f: A => IndexedTimedValue[B, T, BTime])(is: IndexingSemigroup[T]): IndexedTimedValue[B, T, is.|+|[ATime, BTime]] =
    IndexedTimedValue[B, T, is.|+|[ATime, BTime]](f(value).value)
}
