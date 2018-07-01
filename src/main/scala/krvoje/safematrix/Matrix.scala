package krvoje.safematrix

import krvoje.safematrix.natural._

import scala.reflect.ClassTag

trait Matrix[M <: Natural, N <: Natural, T] {
  def +(that: Matrix[M, N, T]): Matrix[M, N, T]
  def -(that: Matrix[M, N, T]): Matrix[M, N, T]
  def *[P <: Natural](that: Matrix[N, P, T]): Matrix[M, P, T]
  def transpose: Matrix[N,M,T]

  def row(m: Natural): NatVect[N, T]
  def col(n: Natural): NatVect[M, T]

  def get(row: Natural, col: Natural): T
}

case class ArrayMatrix[M <: Natural, N <: Natural, T](
  m: M,
  n: N,
  rows: Array[Array[T]]
)(implicit num: Numeric[T], classTag: ClassTag[T]) extends Matrix[M, N, T] {

  // TODO: Type these instead of requiring
  require(rows.length == m.toInt, "Invalid number of rows in matrix")
  for(row <- rows) require(row.length == n.toInt, "Invalid number of cols in matrix")

  override def +(that: Matrix[M, N, T]): Matrix[M, N, T] = rowsOp(num.plus, that)

  override def -(that: Matrix[M, N, T]): Matrix[M, N, T] = rowsOp(num.minus, that)

  override def get(row: Natural, col: Natural): T = rows(row.toInt)(col.toInt)

  private def rowsOp[T](f: (T, T) => T, that: Matrix[M, N, T]): Matrix[M, N, T] = ???/*this.copy(rows = (for {
    row <- Range(0, m.toInt).toArray[Int]
  } yield for {
    col <- Range(0, n.toInt).toArray[Int]
  } yield {
    val left = this.get(Natural(row), Natural(col))
    val right = that.get(Natural(row), Natural(col))
    f(left, right)
  }))*/
  override def *[P <: Natural](that: Matrix[N, P, T]): Matrix[M, P, T] = ???

  override def transpose: Matrix[N, M, T] = ???

  override def row(m: Natural): NatVect[N, T] = ???

  override def col(n: Natural): NatVect[M, T] = ???
}