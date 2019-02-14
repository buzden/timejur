package ru.buzden.timejur.value

final case class TimedValue[+A, +T](value: A, time: T)
