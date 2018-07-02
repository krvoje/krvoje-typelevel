package krvoje.safematrix.natural

import scala.annotation.implicitNotFound
import scala.collection.mutable

sealed trait Natural {
  type N <: Natural

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
  type N = Zero
}

case class Successor[P <: Natural](predecessor: P) extends Natural {
  type N = Successor[predecessor.N]
}

@implicitNotFound("Cannot prove that ${N} has a predecessor")
trait HasPredecessor[N <: Natural] {
  type P <: Natural
  type N = Successor[P]
}
object HasPredecessor {
  implicit def predecessor[Pred <: Natural] = new HasPredecessor[Successor[Pred]] {type P = Pred}
  //implicit def predecessor[X <: Natural, Y <: Natural](implicit prf: X < Y) = new Predecessor[Y] {type Out = P}
}

@implicitNotFound("Cannot compute witness for ${X} + ${Y}")
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

@implicitNotFound("Cannot prove that ${X} < ${Y}")
trait <[X <: Natural, Y <: Natural]
object < {
  implicit def
  `X < SX`[
  X <: Natural
  ] = new <[X, Successor[X]] {}

  implicit def
  `X < Y => X < SY`[
  X <: Natural,
  Y <: Natural,
  ](implicit p1: X < Y) = new <[X, Successor[Y]] {}

  implicit def
  `X = Successor[_] => X.predecessor < X`
  [
  X <: Natural,
  Y <: Successor[X]
  ](implicit x: Y) = new <[x.predecessor.N, Y]{}

}

/** Proof that A <= B */
@implicitNotFound("Cannot prove that ${X} <= ${Y}")
trait <=[X <: Natural, Y <: Natural]
object <= {

  implicit def
  `X < Y => X <= Y`
  [
  X <: Natural,
  Y <: Natural
  ]
  (implicit p: X<Y)=new <=[X, Y]{}

  implicit def
  `X <= X`
  [
  X <: Natural
  ] = new <= [X, X]{}
}

object Natural {
  type One = Successor[Zero]
  type Two = Successor[One]
  type Three = Successor[Two]
  type Four = Successor[Three]

  val Zero = new Zero
  val One = Successor(Zero)
  val Two = Successor(One)
  val Three = Successor(Two)
  val Four = Successor(Three)

  private val cache: mutable.Map[Int, Natural] = mutable.Map.empty[Int, Natural]

  def apply(number: Int): Natural = {
    if(!cache.isDefinedAt(number)) {
      cache.put(number, fromInt(number))
    }

    cache(number)
  }

  implicit def toInt(number: Natural): Int = number.toInt

  implicit def fromInt(number: Int): Natural = {
    require(number >= 0, "Cannot make a natural number from a negative integer value.")

    if(number == 0) {
      Zero
    } else {
      Successor(fromInt(number - 1))
    }
  }

}