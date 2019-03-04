package ru.buzden.iz

sealed trait Nat0 {
  type Sum[N <: Nat0] <: Nat0
  def add[N <: Nat0](n: N): Sum[N]
}

object Nat0 {
  final case object Zero extends Nat0 {
    override type Sum[N <: Nat0] = N
    override def add[N <: Nat0](n: N): Sum[N] = n
  }

  final case class Succ[Prev <: Nat0](p: Prev) extends Nat0 {
    override type Sum[N <: Nat0] = Succ[p.Sum[N]]
    override def add[N <: Nat0](n: N): Succ[p.Sum[N]] = Succ(p.add(n))
  }

  implicit val nat0HasTLMonoid: TwoFacedMonoid[Nat0] = new TwoFacedMonoid[Nat0] {
    override type Empty = Zero.type
    override type |+|[A <: Nat0, B <: Nat0] = A#Sum[B]

    override type EmptyR[R] = R
    override def empty: Zero.type = Zero

    override type CombineR[A, B, R] = R
    override def combine[A <: Nat0, B <: Nat0](a: A, b: B): A#Sum[B] = a `add` b
  }
}
