package ru.buzden.timejur.computation

/**
  * Data structure representing a timed computation (i.e., a computation
  * that spends some model time to get its result) that contains both
  * dynamic time component (depending on the input) and
  * static time component (maximum time that can be spent).
  *
  * Static time component is stored in a value.
  * That is, the following invariant must always hold:
  * `forall a: A :: f(a)._2 <= maxTime`
  *
  * @tparam A input type for the computation
  * @tparam B resulting type of the computation
  * @tparam T type for time
  * @param f function that defines computation **and** spent model time for each input
  * @param maxTime maximum spent time for all possible computation inputs
  */
final case class DuallyTimed[-A, +B, +T](f: A => (B, T), maxTime: T)
// todo to make this not a case class and that f's second component
//  actually returns min of raw f's result and maxTime value
