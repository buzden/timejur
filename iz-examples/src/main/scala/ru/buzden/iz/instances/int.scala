package ru.buzden.iz.instances

import ru.buzden.iz.{IFT, TwoFacedMonoid}
import singleton.ops.+

object int {
  implicit val intHasIndexingMonoid: TwoFacedMonoid[Int] = new TwoFacedMonoid[Int] {
    override type EmptyR[A] = A
    override type CombineR[A, B, C] = IFT[A + B, C]

    override type Empty = 0
    override type |+|[A, B] = (A + B)#OutInt

    override def empty: Empty = 0
    override def combine[A, B](a: A, b: B): CombineR[A, B, A |+| B] = IFT { implicit p =>
      p.value.asInstanceOf
    }
  }
}
