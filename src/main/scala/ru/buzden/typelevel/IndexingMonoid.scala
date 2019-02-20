package ru.buzden.typelevel

trait IndexingMonoid[I] extends IndexingSemigroup[I] {
  type Empty <: I
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
