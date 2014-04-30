package arithmetic

import scala.language.implicitConversions
import S99Int._

class S99Int(val i: Int) {

  /**
   * http://en.wikipedia.org/wiki/Primality_test
   */
  def isPrime: Boolean =
    (i > 1) && (Stream.cons(2, Stream(3, 2)) takeWhile ( _ <= Math.sqrt(i)) forall ( i % _ > 0 ))
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def apply(i: Int) = new S99Int(i)

  val primes = Stream.cons(2, Stream(3, 2)) filter ( _.isPrime )
}
