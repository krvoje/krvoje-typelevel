package krvoje.safematrix.natural

sealed trait NatVect[Len <: Natural, T] {
  def ::(h: T): NatVect[Successor[Len], T] = NVCons(h, this)

  lazy val length: Natural = this match {
    case _: NVNil[T] => Natural.Zero
    case nvc: NVCons[_, T] => Successor(nvc.t.length)
  }
}

class NVNil[T] extends NatVect[Zero, T] {
  override def ::(h: T): NatVect[Successor[Zero], T] = NVCons(h, this)
}
object NVNil {
  def apply[T]: NVNil[T] = new NVNil[T]
}
case class NVCons[TailLen <: Natural, T](h: T, t: NatVect[TailLen, T]) extends NatVect[Successor[TailLen], T]