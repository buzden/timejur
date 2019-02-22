package ru.buzden.timejur.value

final case class IndexedDuallyTimedValue[A, ATime](value: A, time: ATime)
