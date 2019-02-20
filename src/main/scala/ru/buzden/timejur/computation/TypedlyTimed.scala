package ru.buzden.timejur.computation

final case class TypedlyTimed[-A, +B, T, MaxT <: T](f: A => (B, T Refined LessEqual[MaxT]))
