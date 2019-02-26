package ru.buzden.timejur.computation

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.LessEqual

final case class TypedlyTimed[-A, +B, T, MaxT <: T](f: A => (B, T Refined LessEqual[MaxT]))
