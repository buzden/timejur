package ru.buzden.typelevel

trait IndexingMonoid[I] extends IndexingSemigroup[I] {
  type Empty <: I
}

class SimpleIndexingMonoid[I, E <: I, C[A <: I, B <: I] <: I] extends SimpleIndexingSemigroup[I, C] with IndexingMonoid[I] {
  type Empty = E
}

trait IndexingMonoidEmerger[I, A <: I, B <: I] extends IndexingSemigroupEmerger[I, A, B] {
  override val proto: IndexingMonoid[I]
  import proto._

  def empty: Empty
}

object IndexingMonoid {
  object syntax {
    def empty[I, A <: I, B <: I](implicit ime: IndexingMonoidEmerger[I, A, B]): ime.proto.Empty = ime.empty
  }
}
