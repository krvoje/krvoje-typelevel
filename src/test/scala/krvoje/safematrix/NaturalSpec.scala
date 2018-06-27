package krvoje.safematrix

import krvoje.safematrix.natural.Natural._
import krvoje.safematrix.natural.{Natural, Successor, Zero, |+|}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

class NaturalSpec extends Properties("Natural") {

  private lazy val naturalGen = for {
    number <- Gen.chooseNum(0, 100)
    natural <- Natural(number)
  } yield natural

  implicit lazy val arbitraryNatural: Arbitrary[Natural] = Arbitrary(naturalGen)

  property("Zero") = {
    Natural.Zero.toInt == 0
  }

  property("One") = {
    val one = Successor(Natural.Zero)
    one.predecessor == Natural.Zero
    one.toInt == 1

    one != Natural(0)
    one != Natural.Zero
    one == Natural(1)
    one != Natural(2)
  }

  property("Sum of two nats is the sum of their underlying ints") = forAll { (a: Natural, b: Natural) =>
    (a+b).toInt == (a.toInt + b.toInt)
  }

  property("Typelevel stuff - if something's wrong these wont compile") = {
    def zeroPlusZero(implicit sum: Zero |+| Zero) = (Zero + Zero).toInt == 0
    def zeroPlusOne(implicit sum: Successor[Zero] |+| Zero) = (One + Zero).toInt == 1
    def onePlusZero(implicit sum: Zero |+| Successor[Zero]) = (Zero + One).toInt == 1
    def onePlusOne(implicit sum: Successor[Zero] |+| Successor[Zero]) = (One + One).toInt == 2

    zeroPlusZero
    zeroPlusOne
    onePlusZero
    onePlusOne
  }

}
