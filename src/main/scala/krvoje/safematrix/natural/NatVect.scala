package krvoje.safematrix.natural

sealed trait NatVect[Len <: Natural, T] {
  def ::(h: T): NatVect[Successor[Len], T] = NVCons(h, this)
}
class NVNil[T] extends NatVect[Zero, T] {
  override def ::(h: T): NatVect[Successor[Zero], T] = NVCons(h, this)
}
object NVNil {
  def apply[T]: NVNil[T] = new NVNil[T]
}
case class NVCons[Len <: Natural, T](h: T, t: NatVect[Len, T]) extends NatVect[Successor[Len], T]