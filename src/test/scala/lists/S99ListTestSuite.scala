package lists

import org.scalatest.FunSuite
import lists.S99List._

/**
 * Created by Adrian on 04.04.2014.
 */
class S99ListTestSuite extends FunSuite {

  trait TestSets {
    val emptyList = List()
    val list1 = List(1)
    val list1LastElem = Some(1)

    val list2 = List(1, 3)
    val list2LastElem = Some(3)

    val list3 = List(1, 1, 2, 3, 5, 8)
    val list3LastElem = Some(8)
    val list3PenultimateElem = Some(5)

    val sizeOfList3 = list3.size

    val list3Reverted = List(8, 5, 3, 2, 1, 1)

    val list14 = List('a, 'b, 'c, 'c, 'd)
    val list14x2 = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    val list14x3 = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

    val mixedList14 = List(1, "a", "b")
    val mixedList14x2 = List(1, 1, "a", "a", "b", "b")

    val list8 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val list8Compressed = List('a, 'b, 'c, 'a, 'd, 'e)

    val list8Packed = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

    val list8Encoded = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

    val list8EncodedModified = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

    val list16 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val list16drop3th = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    val list16drop100th = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val list16drop11th = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j)

    val list16split3 = (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

    val list16Sliced3to7 = List('d, 'e, 'f, 'g)
    val list16Sliced0to4 = List('a, 'b, 'c)

    val list16Rotated3Pos = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    val list16Rotated2Neg = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

  }

  test("'last' on empty list ") {
    new TestSets {
      assert(last(emptyList) == None)
    }
  }

  test("'last' on one element") {
    new TestSets {
      assert(last(list1) === list1LastElem)
    }
  }

  test("'last' on more than one element") {
    new TestSets {
      assert(last(list3) === list3LastElem)
    }
  }

  test("'penultimate' on more than one element") {
    new TestSets {
      assert(penultimate(list3) == list3PenultimateElem)
    }
  }

  test("'penultimate' on empty list ") {
    new TestSets {
      assert(penultimate(emptyList) == None)
    }
  }

  test("'penultimate' on list of size one") {
    new TestSets {
      assert(penultimate(list1) === None)
    }
  }

  test("'nth' with a negative index") {
    assert(nth(-4, List(1, 55, 89, 43, 43, 89, 65)) === None)
  }

  test("'nth' on some list with size bigger that one") {
    assert(nth(4, List(1, 55, 89, 23, 43, 11, 65)) === Some(43))
  }

  test("'nth' with index bigger that the size of the list") {
    assert(nth(100, List(1, 55, 89, 23, 43, 11, 65)) === None)
  }

  test("'length' on some list with size bigger that one") {
    new TestSets {
      assert(length(list3) === sizeOfList3)
    }
  }

  test("'length' on empty list") {
    assert(length(List()) === 0)
  }

  test("'reverse' a list with size bigger that one") {
    new TestSets {
      assert(reverse(list3) === list3Reverted)
    }
  }

  test("'flatten' a list with size bigger that one") {
    assert(flatten(List(21, 45, List(4, 6, 8), List(1, List(5, 7)), List(8, List(List("A", "B"))))) ===
      List(21, 45, 4, 6, 8, 1, 5, 7, 8, "A", "B"))
  }

  test("'compress' a list with size bigger that one") {
    new TestSets {
      assert(compress(list8) === list8Compressed)
    }
  }

  test("'pack' a list with size bigger that one") {
    new TestSets {
      assert(pack(list8) === list8Packed)
    }
  }

  test("'encode' a list with size bigger that one") {
    new TestSets {
      assert(encode(list8) === list8Encoded)
    }
  }

  test("'encodeModified' a list with size bigger that one") {
    new TestSets {
      assert(encodeModified(list8) === list8EncodedModified)
    }
  }

  test("'decode' a list with size bigger that one") {
    new TestSets {
      assert(decode(list8Encoded) === list8)
    }
  }

  test("'encodeDirect' a list with size bigger that one") {
    new TestSets {
      assert(encodeDirect(list8) === list8Encoded)
    }
  }

  test("'isPalindrome' MONOM is true") {
    assert(isPalindrome(List("M", "O", "N", "O", "M")))
  }

  test("'isPalindrome' PALINDROm is false") {
    assert(!isPalindrome(List("P", "A", "L", "I", "N", "D", "R", "O", "m")))
  }

  test("'duplicate' on more than one elem") {
    new TestSets {
      assert(duplicate(list14) === list14x2)
      assert(duplicate(mixedList14) === mixedList14x2)
    }
  }

  test("'duplicateN' on more than one elem") {
    new TestSets {
      assert(duplicateN(3, list14) === list14x3)
    }
  }

  test("'drop' on more than one elem") {
    new TestSets {
      assert(drop(3, list16) === list16drop3th)
    }
  }

  test("'drop' 100th elem on a list of size smaller than 100") {
    new TestSets {
      assert(drop(100, list16) === list16)
    }
  }

  test("'drop' 11th elem on a list of size 11") {
    new TestSets {
      assert(drop(11, list16) === list16drop11th)
    }
  }

  test("'drop' on an empty list") {
    new TestSets {
      assert(drop(3, emptyList) === emptyList)
    }
  }

  test("'drop' 1st elem returns empty list") {
    new TestSets {
      assert(drop(1, list16) === emptyList)
    }
  }

  test("'split' on more than one elem") {
    new TestSets {
      assert(split(3, list16) === list16split3)
    }
  }

  test("'split' on empty list") {
    new TestSets {
      assert(split(3, emptyList) === (List(), List()))
    }
  }

  test("'split' with index greater that size of the list") {
    new TestSets {
      assert(split(100, list16) === (list16, List()))
    }
  }

  test("'slice' on some list with size bigger than one") {
    new TestSets {
      assert(slice(3, 7, list16) === list16Sliced3to7)
    }
  }

  test("'slice' with first=last") {
    new TestSets {
      assert(slice(3, 3, list16) === emptyList)
    }
  }

  test("'slice' with first<0 and 0<last<size") {
    new TestSets {
      assert(slice(-3, 3, list16) === list16Sliced0to4)
    }
  }

  test("'slice' with first<0 and last>size") {
    new TestSets {
      assert(slice(-3, 100, list16) === list16)
    }
  }

  test("'slice' with first<0 and last<0") {
    new TestSets {
      assert(slice(-3, -1, list16) === emptyList)
    }
  }

  test("'slice' with first<last") {
    new TestSets {
      assert(slice(3, 1, list16) === emptyList)
    }
  }

  test("'rotate' +3") {
    new TestSets {
      assert(rotate(3, list16) === list16Rotated3Pos)
    }
  }

  test("'rotate' -2") {
    new TestSets {
      assert(rotate(-2, list16) === list16Rotated2Neg)
    }
  }

  test("'removeAt' middle") {
    assert( removeAt(1, List('a, 'b, 'c, 'd)) === (List('a, 'c, 'd), 'b) )
  }

  test("'removeAt' first") {
    assert( removeAt(0, List('a, 'b, 'c, 'd)) === (List('b, 'c, 'd), 'a) )
  }

  test("'removeAt' last") {
    assert( removeAt(3, List('a, 'b, 'c, 'd)) === (List('a, 'b, 'c), 'd) )
  }

  test("'removeAt' out of bounds left") {
    intercept[ArrayIndexOutOfBoundsException]{ removeAt(-1, List('a, 'b, 'c, 'd)) }
  }

  test("'removeAt' out of bounds right") {
    intercept[ArrayIndexOutOfBoundsException]{ removeAt(4, List('a, 'b, 'c, 'd)) }
  }

  test("'insertAt' middle") {
    assert( insertAt('new, 1, List('a, 'b, 'c, 'd)) === List('a, 'new, 'b, 'c, 'd) )
  }

  test("'insertAt' first") {
    assert( insertAt('new, 0, List('a, 'b, 'c, 'd)) === List('new, 'a, 'b, 'c, 'd) )
  }

  test("'insertAt' last") {
    assert( insertAt('new, 4, List('a, 'b, 'c, 'd)) === List('a, 'b, 'c, 'd, 'new) )
  }

  test("'insertAt' out of bounds left") {
    intercept[ArrayIndexOutOfBoundsException]{ insertAt('new, -1, List('a, 'b, 'c, 'd)) }
  }

  test("'insertAt' out of bounds right") {
    intercept[ArrayIndexOutOfBoundsException]{ insertAt('new, 5, List('a, 'b, 'c, 'd)) }
  }

  test("'range' from 4 to 9") {
    assert( range(4, 9) === List(4, 5, 6, 7, 8, 9) )
  }

  test("'randomSelect' from a list") {
    val list: List[Int] = List(4, 5, 6, 7, 8, 9)

    val select = randomSelect(2, list)
    assert( select.forall(list.contains(_)) )
    assert( select.length == 2 )
  }

  test("'lotto' 6 from 49") {
    val result: List[Int] = lotto(6, 49)

    assert(result.forall(i => i >= 1 && i <= 49))
    assert(result.distinct.length === result.length)
  }

  test("'randomPermute' on some list") {
    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g)
    val result = randomPermute(list)

    assert(list.size === result.size)
    assert(result.distinct.length === result.length)
  }
}
