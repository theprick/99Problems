package arithmetic

import scala.language.implicitConversions
import scala.annotation.tailrec
import arithmetic.S99Int._

class S99Int(val value: Int) {

  /**
   * P31 (**) Determine whether a given integer number is prime.
   * http://en.wikipedia.org/wiki/Primality_test
   */
  def isPrime: Boolean =
    (value > 1) && (Stream.cons(2, Stream.from(3, 2)) takeWhile (_ <= Math.sqrt(value)) forall (value % _ > 0))


  /**
   * P33 (*) Determine whether two positive integer numbers are coprime.
   * Two numbers are coprime if their greatest common divisor equals 1
   */
  def isComprimeTo(n: Int): Boolean = gcd(value, n) == 1

  /**
   * P34 (**) Calculate Euler's totient function phi(m).
   * Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
   * http://en.wikipedia.org/wiki/Totient
   * @return the multitude of numbers less than value, and which have no common divisor with it
   */
  def totient: Int = Stream.from(1) takeWhile (_ <= value) count isComprimeTo
  //(1 to start) filter { start.isCoprimeTo(_) } length
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  implicit def s99Int2Int(i: S99Int): Int = i.value

  def apply(i: Int) = new S99Int(i)

  val primes = Stream.cons(2, Stream(3, 2)) filter (_.isPrime)

  /**
   * P32 (**) Determine the greatest common divisor of two positive integer numbers.
   * http://en.wikipedia.org/wiki/Greatest_common_divisor
   */
  @tailrec def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
}
