package krvoje.safematrix.natural
sealed trait NatMatrix[Rows <: Natural, Cols <: Natural, T] {
  def ::(h: NatMatrix.Row[Cols, T]): NatMatrix[Successor[Rows], Cols, T] = NMCons(h, this)
}
class NMNil[Cols <: Natural, T] extends NatMatrix[Zero, Cols, T] {
  override def ::(h: NatMatrix.Row[Cols, T]): NatMatrix[Successor[Zero], Cols, T] = NMCons(h, this)
}
object NMNil {
  def apply[Cols <: Natural, T]: NMNil[Cols, T] = new NMNil[Cols, T]
}
case class NMCons[Rows <: Natural, Cols <: Natural, T](h: NatMatrix.Row[Cols, T], t: NatMatrix[Rows, Cols, T]) extends NatMatrix[Successor[Rows], Cols, T]

case object NatMatrix {
  type Row[Cols <: Natural, T] = NatVect[Cols, T]
}