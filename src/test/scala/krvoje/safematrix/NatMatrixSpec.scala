package krvoje.safematrix

import krvoje.safematrix.natural._
import org.scalacheck._

class NatMatrixSpec extends Properties("Natural") {

  property("Typelevel stuff - if something's wrong these wont compile") = {
    def compute[Rows <: Natural, Cols <: Natural, T](mtx: NatMatrix[Rows, Cols, T]) = mtx.toString.nonEmpty

    compute[Zero, Zero, String](NMNil[Zero, String])
    compute[Natural.One, Natural.One, String](("2" :: NVNil[String]) :: NMNil[Natural.One, String])

    compute[Natural.One, Natural.Two, String](("3" :: "2" :: NVNil[String])::NMNil[Natural.Two, String])
    compute[Natural.Two, Natural.One, String](
      ("2" :: NVNil[String]) ::
        ("2" :: NVNil[String]) ::
        NMNil[Natural.One, String])

    compute[Natural.Two, Natural.Three, String](
      ("3" :: "2" ::"1" :: NVNil[String]) ::
        ("3" :: "2" ::"1" :: NVNil[String]) ::
      NMNil[Natural.Three, String])
    compute[Natural.Three, Natural.Two, String](
      ("3" :: "2" :: NVNil[String]) ::
        ("3" :: "2" :: NVNil[String]) ::
        ("3" :: "2" :: NVNil[String]) ::
        NMNil[Natural.Two, String])

    true == true
  }

}
