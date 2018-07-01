package krvoje.safematrix

import krvoje.safematrix.natural._
import org.scalacheck._

class NatVectSpec extends Properties("Natural") {

  property("Typelevel stuff - if something's wrong these wont compile") = {
    def compute[Len <: Natural, T](vect: NatVect[Len, T]) = vect.toString.nonEmpty

    compute[Zero, String](NVNil[String])
    compute[Successor[Zero], String]("2" :: NVNil[String])
    compute[Successor[Successor[Zero]], String]("3" :: "2" :: NVNil[String])

    compute[Zero, Int](NVNil[Int])
    compute[Successor[Zero], Int](2 :: NVNil[Int])
    compute[Successor[Successor[Zero]], Int](3 :: 2 :: NVNil[Int])

    true == true
  }

}
