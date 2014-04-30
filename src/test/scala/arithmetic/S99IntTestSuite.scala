package arithmetics

import arithmetic.S99Int._
import org.scalatest.FunSuite

/**
 * Created by Adrian on 13.04.2014.
 */
class S99IntTestSuite extends FunSuite {
  test("'isPrime' true") {
      assume(7.isPrime)
  }

  test("'isPrime' false") {
      assume(!8.isPrime)

  }
}
