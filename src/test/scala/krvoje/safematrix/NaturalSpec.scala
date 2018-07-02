package krvoje.safematrix

import krvoje.safematrix.natural._
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
    import Natural.{One, Two, Three, Four}
    def zeroPlusZero(implicit sum: Zero |+| Zero) = true
    def zeroPlusOne(implicit sum: Successor[Zero] |+| Zero) = true
    def onePlusZero(implicit sum: Zero |+| Successor[Zero]) = true
    def onePlusOne(implicit sum: Successor[Zero] |+| Successor[Zero]) = true

    def zeroLTEQZero(implicit proof: Zero <= Zero) = true
    def zeroLTEQOne(implicit proof: Zero <= One) = true
    def zeroLTEQThree(implicit proof: Zero <= Three) = true

    //def oneLTEQZero(implicit proof: <= [Natural.One, Zero]) = true
    def oneLTEQOne(implicit proof: One <= One) = true
    def oneLTEQThree(implicit proof: One <= Three) = true
    def oneLTEQFour(implicit proof: One <= Four) = true

    def twoLTEQTwo(implicit proof: Two <= Two) = true
    def twoLTEQThree(implicit proof: Two <= Natural.Three) = true
    def twoLTEQFour(implicit proof: Two <= Four) = true

    //def zeroLTZero(implicit proof: Zero < Zero) = true
    def zeroLTOne(implicit proof: Zero < One) = true
    def zeroLTThree(implicit proof: Zero < Three) = true
    //def oneLTZero(implicit proof: < [Natural.One, Zero]) = true
    //def oneLTOne(implicit proof: One < One) = true
    def oneLTThree(implicit proof: One < Three) = true
    def oneLTFour(implicit proof: One < Four) = true
    //def twoLTTwo(implicit proof: Two < Two) = true
    def twoLTThree(implicit proof: Two < Natural.Three) = true
    def twoLTFour(implicit proof: Two < Four) = true


    zeroPlusZero
    zeroPlusOne
    onePlusZero
    onePlusOne

    zeroLTEQZero
    zeroLTEQOne
    zeroLTEQThree
    //oneLTEQZero // Should not compile
    oneLTEQOne
    oneLTEQThree
    oneLTEQFour
    //twoLTEQTwo
    twoLTEQThree
    twoLTEQFour

    //zeroLTZero
    zeroLTOne
    zeroLTThree
    //oneLTZero // Should not compile
    //oneLTOne
    oneLTThree
    oneLTFour
    //twoLTTwo
    twoLTThree
    twoLTFour
  }

}
