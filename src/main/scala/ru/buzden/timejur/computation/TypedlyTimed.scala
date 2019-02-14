package ru.buzden.timejur.computation

import ru.buzden.timejur.computation.TypedlyTimed.<=

// todo MaxT should be a literal type
final case class TypedlyTimed[-A, +B, T, MaxT <: T](f: A => (B, T <= MaxT))

object TypedlyTimed {
  type <=[A, B] = A Refined LessEqual[B]
}