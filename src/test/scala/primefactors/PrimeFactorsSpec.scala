package primefactors

import org.specs2.mutable.Specification

class PrimeFactorsSpec extends Specification {
  import PrimeFactors._

  "primefactors.primeFactors" should {

    "throw an Error for zero" in {
      primeFactors(0) must throwAn[Error].like { case e => e.getMessage must be equalTo "division by zero" }
    }

    "generate an empty map for one" in {
      primeFactors(1) must beEmpty
    }

    "generate a Vector(2 -> 1) for 2" in {
      primeFactors(2) must be equalTo Vector(2 -> 1)
    }

    "generate a Vector(3 -> 1) for 3" in {
      primeFactors(3) must be equalTo Vector(3 -> 1)
    }

    "generate a Vector(2 -> 2) for 4" in {
      primeFactors(4) must be equalTo Vector(2 -> 2)
    }

    "generate a Vector(2 -> 1, 3 -> 1) for 6" in {
      primeFactors(6) must be equalTo Vector(2 -> 1, 3 -> 1)
    }

    "generate a Vector(2 -> 3) for 8" in {
      primeFactors(8) must be equalTo Vector(2 -> 3)
    }

    "generate a Vector(2 -> 2, 3 -> 1) for 12" in {
      primeFactors(12) must be equalTo Vector(2 -> 2, 3 -> 1)
    }

    "generate a Vector(2 -> 9, 3 -> 7) for 1119744" in {
      primeFactors(1119744) must be equalTo Vector(2 -> 9, 3 -> 7)
    }

    "generate a Vector(2 -> 1, 3 -> 1, 79 -> 1, 26041 -> 1) for 12343434" in {
      primeFactors(12343434) must be equalTo Vector(2 -> 1, 3 -> 1, 79 -> 1, 26041 -> 1)
    }

    "generate a Vector(-1 -> 1, 2 -> 2, 3 -> 1) for -12" in {
      primeFactors(-12) must be equalTo Vector(-1 -> 1, 2 -> 2, 3 -> 1)
    }
  }

  "primefactors.primeFactorsLinear" should {

    "throw an Error for zero" in {
      primeFactorsLinear(0) must throwAn[Error].like { case e => e.getMessage must be equalTo "division by zero" }
    }

    "generate an empty list for one" in {
      primeFactorsLinear(1) must beEmpty
    }

    "generate a Vector(2) for 2" in {
      primeFactorsLinear(2) must be equalTo Vector(2)
    }

    "generate a Vector(3) for 3" in {
      primeFactorsLinear(3) must be equalTo Vector(3)
    }

    "generate a Vector(2, 2) for 4" in {
      primeFactorsLinear(4) must be equalTo Vector(2, 2)
    }

    "generate a Vector(2, 3) for 6" in {
      primeFactorsLinear(6) must be equalTo Vector(2, 3)
    }

    "generate a Vector(2, 2, 2) for 8" in {
      primeFactorsLinear(8) must be equalTo Vector(2, 2, 2)
    }

    "generate a Vector(2, 2, 3) for 12" in {
      primeFactorsLinear(12) must be equalTo Vector(2, 2, 3)
    }

    "generate a Vector(2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3) for 1119744" in {
      primeFactorsLinear(1119744) must be equalTo Vector(2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3)
    }

    "generate a Vector(2, 3, 79, 26041) for 12343434" in {
      primeFactorsLinear(12343434) must be equalTo Vector(2, 3, 79, 26041)
    }

    "generate a Vector(-1, 2, 2, 3) for -12" in {
      primeFactorsLinear(-12) must be equalTo Vector(-1, 2, 2, 3)
    }

  }

  "primefactors.linear2tuple" should {

    "convert [1, 2, 3] into [1 -> 1, 2 -> 1, 3 -> 1]" in {
      linear2tupled(Vector(1, 2, 3)) must be equalTo Vector(1 -> 1, 2 -> 1, 3 -> 1)
    }

    "convert [1, 1, 3] into [1 -> 2, 3 -> 1]" in {
      linear2tupled(Vector(1, 1, 3)) must be equalTo Vector(1 -> 2, 3 -> 1)
    }

    "convert [] into []" in {
      linear2tupled(Vector()) must beEmpty
    }

    "convert [0] into [0 -> 1]" in {
      linear2tupled(Vector(0)) must be equalTo Vector(0 -> 1)
    }
  }

  "primefactors.tupled2linear" should {

    "convert [1 -> 1, 2 -> 1, 3 -> 1] into [1, 2, 3]" in {
      tupled2linear(Vector(1 -> 1, 2 -> 1, 3 -> 1)) must be equalTo Vector(1, 2, 3)
    }

    "convert [1 -> 2, 3 -> 1] into [1, 1, 3]" in {
      tupled2linear(Vector(1 -> 2, 3 -> 1)) must be equalTo Vector(1, 1, 3)
    }

    "convert [] into []" in {
      tupled2linear(Vector()) must beEmpty
    }

    "convert [0 -> 1] into [0]" in {
      tupled2linear(Vector(0 -> 1)) must be equalTo Vector(0)
    }
  }
}
