package krvoje.safematrix.natural

import scala.collection.mutable

sealed trait Natural {
  type Underlying <: Natural

  def successor: Natural = Successor(this)

  def +(other: Natural): Natural = this match {
    case _: Zero => other
    case Successor(thisPrev) => other match {
      case _: Zero => this
      case Successor(otherPrev) => Successor(Successor(thisPrev + otherPrev))
    }
  }

  lazy val toInt: Int = this match {
    case _: Zero => 0
    case Successor(prev) => prev.toInt + 1
  }

}

class Zero extends Natural {
  type Underlying = Zero
}

case class Successor[P <: Natural](predecessor: P) extends Natural {
  type Underlying = Successor[P]
}

trait |+|[X <: Natural, Y <: Natural] {
  type Out <: Natural
}
object |+| {
  implicit def
  `0 + Y = Y`
  [Y <: Natural]: Zero |+| Y = new (Zero |+|  Y) {type Z = Y}

  implicit def
  `X + SY = Z => SX + Y = Z`[
  X <: Natural,
  Y <: Natural
  ](implicit sum: X |+| Successor[Y]): Successor[X] |+| Y = new (Successor[X] |+| Y) {type Out = Successor[sum.Out]}
}


object Natural {
  type One = Successor[Zero]
  type Two = Successor[One]
  type Three = Successor[Two]

  val Zero = new Zero
  val One = Successor(Zero)
  val Two = Successor(One)
  val Three = Successor(Two)

  private val cache: mutable.Map[Int, Natural] = mutable.Map.empty[Int, Natural]

  def apply(number: Int): Natural = {
    if(!cache.isDefinedAt(number)) {
      cache.put(number, fromInt(number))
    }

    cache(number)
  }

  private def fromInt(number: Int): Natural = {
    require(number >= 0, "Cannot make a natural number from a negative integer value.")

    if(number == 0) {
      Zero
    } else {
      Successor(fromInt(number - 1))
    }
  }

}