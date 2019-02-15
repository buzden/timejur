package ru.buzden.timejur.value

import singleton.ops.+

final case class IndexedTimedValue[A, T, ATime <: T with Singleton](value: A) {
  def flatMap[B, BTime <: T with Singleton](f: A => IndexedTimedValue[B, T, BTime]): IndexedTimedValue[B, T, ATime + BTime] =
    IndexedTimedValue[B, T, ATime + BTime](f(value).value)
}
