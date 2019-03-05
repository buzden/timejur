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

    override def leftIdentityLaw[B <: Nat0]: B =:= B = implicitly

    override def rightIdentityLaw[A <: Nat0]: A#Sum[Nat0.Zero.type] =:= A = ???

    override def associativityLaw[A <: Nat0, B <: Nat0, C <: Nat0]: A#Sum[B]#Sum[C] =:= A#Sum[B#Sum[C]] = ???

    override def empty: Empty = Zero

    override def combine[A <: Nat0, B <: Nat0](a: A, b: B): A |+| B = a `add` b
  }
}
