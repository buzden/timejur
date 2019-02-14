package ru.buzden.timejur.computation

/**
  * Data structure representing a timed computation (i.e., a computation
  * that spends some model time to get its result) where
  * time is known only dynamically, i.e. only for each particular input
  * of the computation.
  *
  * @tparam A input type for the computation
  * @tparam B resulting type of the computation
  * @tparam T type for time
  * @param f function that defines computation **and** spent model time for each input
  */
final case class DynamicallyTimed[-A, +B, +T](f: A => (B, T))
