package arithmetic

import scala.language.implicitConversions
import scala.annotation.tailrec

class S99Int(val value: Int) {

  /**
   * http://en.wikipedia.org/wiki/Primality_test
   */
  def isPrime: Boolean =
    (value > 1) && (Stream.cons(2, Stream(3, 2)) takeWhile ( _ <= Math.sqrt(value)) forall ( value % _ > 0 ))
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  implicit def s99Int2Int(i: S99Int): Int = i.value

  def apply(i: Int) = new S99Int(i)

  val primes = Stream.cons(2, Stream(3, 2)) filter ( _.isPrime )

  /**
   * P32 (**) Determine the greatest common divisor of two positive integer numbers.
   * http://en.wikipedia.org/wiki/Greatest_common_divisor
   */
  @tailrec def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)
}
