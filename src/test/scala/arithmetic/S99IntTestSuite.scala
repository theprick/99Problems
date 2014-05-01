package arithmetic

import arithmetic.S99Int._
import org.scalatest.FunSuite

/**
 * Created by Adrian on 13.04.2014.
 */
class S99IntTestSuite extends FunSuite {
  test("7 is prime") {
    assume(7.isPrime)
  }

  test("8 is not prime") {
    assume(!8.isPrime)
  }

  test("gdc(48, 18) = 6") {
    assert(gcd(48, 18) === 6)
  }

  test("gdc(30, 12) = 6") {
    assert(gcd(30, 12) === 6)
  }

  test("gdc(77, 55) = 11") {
    assert(gcd(77, 55) === 11)
  }

  test("gdc(77, 2) = 1") {
    assert(gcd(77, 2) === 1)
  }

  test("gdc(3, 76) = 1") {
    assert(gcd(3, 76) === 1)
  }

  test("gdc(3, 76) = 1 with conversion from S99Int to Int") {
    assert(gcd(new S99Int(3), 76) === 1)
  }
}
