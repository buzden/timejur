package ru.buzden

package object typelevel {
  /** Simple type alias to make enormously loooong signatures shorter */
  type X[T] = T with Singleton
}
