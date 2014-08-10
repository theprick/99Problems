package arithmetic

import arithmetic.S99Int._
import org.scalatest.FunSuite

/**
 * This file was created by Popescu Adrian-Dumitru on 13.04.2014.
 */
class S99IntTestSuite extends FunSuite {
  test("7 is prime") {
    assert(7.isPrime)
  }

  test("8 is not prime") {
    assert(!8.isPrime)
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

  test("9 and 4 are coprime") {
    assert(9.isComprimeTo(4))
  }

  test("21 and 15 are not coprime") {
    assert(!21.isComprimeTo(15))
  }

  test("totient of 10 is 4") {
    assert(10.totient === 4)
  }

  test("primeFactors of 315") {
    assert(315.primeFactors === List(3, 3, 5, 7))
  }
}
