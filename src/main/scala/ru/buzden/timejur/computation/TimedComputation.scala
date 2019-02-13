package ru.buzden.timejur.computation

case class TimedComputation[A, B, T](computation: A => B, time: T)

object TimedComputation {
}
