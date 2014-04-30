package lists

import scala.annotation.tailrec

/**
 * Created by Adrian on 05.04.2014.
 */
object S99List {

  /**
   * P01 Find the last element of a list.
   */
  @tailrec def last[T](list: List[T]): Option[T] = list match {
    case List() => None
    case head :: List() => Some(head)
    case head :: tail => last(tail)
  }

  /**
   * P02 Find the last but one element of a list.
   */
  @tailrec def penultimate(list: List[Int]): Option[Int] = list match {
    case List() | List(_) => None
    case head :: _ :: List() => Some(head)
    case head :: tail => penultimate(tail)
  }

  /**
   * P03 Find the Kth element of a list. (0 based)
   */
  def nth[T](pos: Int, list: List[T]): Option[T] = {
    @tailrec def findElementAtPos(currentPos: Int, list: List[T]): Option[T] = list match {
      case Nil => None
      case head :: _ if currentPos == pos => Some(head)
      case head :: tail => findElementAtPos(currentPos + 1, tail)
    }

    if (pos < 0) None
    else findElementAtPos(0, list)
  }

  /**
   * P04 Find the number of elements of a list.
   */
  def length[T](list: List[T]): Int = {
    @tailrec def length(acc: Int, list: List[T]): Int = list match {
      case Nil => acc
      case _ :: tail => length(acc + 1, tail)
    }
    // Solution 1: recursive call
    //length(0, list)
    // Solution 2: built in foldLeft function
    //list.foldLeft(0)((a, _) => a + 1)
    // Solution 3: my custom foldLeft function
    foldLeft(list, 0)((a, _) => a + 1)
  }

  /**
   * P05 Reverse a list.
   */
  def reverse[T](list: List[T]): List[T] = {
    list.foldRight(List[T]())((i, a) => {
      a ++ List(i)
    })
  }

  /**
   * P06 Find out whether a list is a palindrome.
   */
  def isPalindrome[T](list: List[T]): Boolean = {
    @tailrec def isPalindrome(head: Int, tail: Int): Boolean =
      if (head == tail) true
      else if (nth(head, list).get != nth(tail, list).get) false
      else isPalindrome(head + 1, tail - 1)

    isPalindrome(0, length(list) - 1)
  }

  /**
   * P07 Flatten a nested list structure.
   */
  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (head: List[Any]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => List(head) ++ flatten(tail)
  }

  /**
   * P08 Eliminate consecutive duplicates of list elements.
   */
  def compress(list: List[Symbol]): List[Symbol] = {
    @tailrec def compress(list: List[Symbol], elem: Symbol, acc: List[Symbol]): List[Symbol] =
      list match {
        case List() => acc
        case head :: tail =>
          if (head == elem)
            compress(tail, elem, acc)
          else
            compress(tail, head, acc ++ List(head))
      }
    compress(list, Symbol(""), List[Symbol]())
  }

  /**
   * P09 Pack consecutive duplicates of list elements into sublists.
   */
  def pack[T](list: List[T]): List[List[T]] = {
    @tailrec def pack(list: List[T], elem: T, acc: List[List[T]], partial: List[T]): List[List[T]] =
      list match {
        case Nil => acc ++ List(partial)
        case head :: tail =>
          if (head == elem) {
            pack(tail, elem, acc, partial ++ List(head))
          } else {
            pack(tail, head, acc ++ List(partial), List[T](head))
          }
      }
    pack(list, nth(0, list).get, List[List[T]](), List[T]())
  }

  /**
   * P10 Run-length encoding of a list. (use P09)
   */
  def encode[T](list: List[T]): List[(Int, T)] = pack(list).map {
    elem => (length(elem), last(elem).get)
  }

  /**
   * P11 Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied
   * into the result list. Only elements with duplicates are transferred as (N, E) terms.
   */
  def encodeModified[T](list: List[T]): List[Any] = pack(list).map {
    case List(elem) => elem
    case elem => (length(elem), last(elem).get)
  }

  /**
   * P12 Decode a run-length encoded list.
   */
  def decode[T](list: List[(Int, T)]): List[T] = {
    @tailrec def decode(list: List[(Int, T)], acc: List[T]): List[T] = {
      list match {
        case Nil => acc
        case (count, elem) :: tail =>
          decode(tail, acc ++ (for (_ <- 1 to count) yield elem).toList)
      }
    }
    decode(list, List[T]())
  }

  //P13 Run-length encoding of a list.
  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    @tailrec
    def encodeDirect(list: List[T], elem: T, acc: List[(Int, T)], count: Int): List[(Int, T)] =
      list match {
        case List() => acc ++ List((count, elem))
        case head :: tail =>
          if (head == elem) {
            encodeDirect(tail, elem, acc, count + 1)
          } else {
            encodeDirect(tail, head, acc ++ List((count, elem)), 1)
          }
      }
    encodeDirect(list, nth(0, list).get, List[(Int, T)](), 0)
  }

  /**
   * P14 Duplicate the elements of a list.
   */
  def duplicate[T](list: List[T]): List[T] =
    foldLeft(list, List[T]()) {
      (acc, elem) => acc ++ List(elem, elem)
    }

  def foldLeft[T, B](list: List[T], acc: B)(op: (B, T) => B): B = {
    @tailrec def foldLeft(list: List[T], acc: B): B = {
      list match {
        case Nil => acc
        case head :: tail =>
          foldLeft(tail, op(acc, head))
      }
    }
    foldLeft(list, acc)
  }

  /**
   * P15 Duplicate the elements of a list a given number of times
   */
  def duplicateN[T](n: Int, list: List[T]): List[T] =
    foldLeft(list, List[T]()) {
      (acc, elem) => acc ++ (for (_ <- 1 to n) yield elem).toList
    }

  /**
   * P16 Drop every Nth element from a list.
   */
  def drop[T](n: Int, list: List[T]): List[T] = {
    @tailrec def drop(list: List[T], count: Int, acc: List[T]): List[T] = {
      list match {
        case List() => acc
        case head :: tail =>
          if(count == n)
            drop(tail, 1, acc)
          else //(count < n)
            drop(tail, count + 1, acc ++ List(head))
      }
    }
    drop(list, 1, List())
  }

  /**
   * P17 Split a list into two parts.
   * The length of the first part is given.
   */
  def split[T](idx: Int, list: List[T]) = {
    @tailrec def split(list: List[T], acc: List[T], count: Int): (List[T], List[T]) = {
      list match {
        case List() => (acc, Nil)
        case head :: tail =>
          if(count == idx)
            (acc, List(head) ++ tail)
          else //count < idx
            split(tail, acc ++ List(head), count + 1)
      }
    }
    split(list, List(), 0)
  }

  /**
   * P18 Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements from and including the
   * Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
   */
  def slice[T](first: Int, last: Int, list: List[T]) = {
    @tailrec def slice(list: List[T], acc: List[T], count: Int): List[T] =
      list match {
        case List() => acc
        case head::tail =>
          if(count == last){
            acc
          } else if(count < first || count > last) {
            slice(tail, acc, count + 1)
          } else { // first <= count < last
            slice(tail, acc ++ List(head), count + 1)
          }
      }

    if(last<first)
      List()
    else
      slice(list, List(), 0)
  }

  /**
   * P19 Rotate a list N places to the left.
   */
  def rotate[T](places: Int, list: List[T]) = {
    @tailrec def rotate(places: Int, list: List[T], acc: List[T], count: Int): List[T] = {
      list match {
        case List() => acc
        case head :: tail =>
          if(count == places) {
            tail ++ acc ++ List(head)
          } else {
            rotate(places, tail, acc ++ List(head), count + 1)
          }
      }
    }
    // places is < 0 means rotate to right |places| starting from end of the list
    // this is similar to: rotate to left size(list) - |places|
    if(places >= 0)
      rotate(places, list, List(), 1)
    else
      rotate(length(list) - (-1) * places, list, List(), 1)
  }

  /**
   * P20 Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple. Elements are numbered from 0.
   */
  def removeAt[T](removeIdx: Int, list: List[T]) = {
    @tailrec def removeAt(list: List[T], i: Int, acc: List[T]): (List[T], T)  =
      list match {
        case head :: List() => (acc, head)
        case head :: tail =>
          if(i == removeIdx) (acc ++ tail, head)
          else removeAt(tail, i + 1, acc ++ List(head))
      }

    if(removeIdx < 0 || removeIdx >= length(list))
      throw new ArrayIndexOutOfBoundsException(s"${removeIdx} is out of bounds.")

    removeAt(list, 0, List())
  }

  /**
   * P21 Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple. Elements are numbered from 0.
   */
  def insertAt[T](newElem: T, insertIdx: Int, list: List[T]) = {
    @tailrec def insertAt(list: List[T], i: Int, acc: List[T]): List[T] =
      list match {
        case List() => acc ++ List(newElem)
        case head :: tail =>
          if(i == insertIdx) {
            acc ++ List(newElem) ++ List(head) ++ tail
          } else {
            insertAt(tail, i + 1, acc ++ List(head))
          }
      }

    if(insertIdx < 0 || insertIdx > length(list))
      throw new ArrayIndexOutOfBoundsException(s"${insertIdx} is out of bounds.")

    // call recursive function defined above
    //insertAt(list, 0, List())

    // Short version using the split function defined for P17
    val splitList = split(insertIdx, list)
    splitList._1 ++ List(newElem) ++ splitList._2
  }

  /**
   * P22 Create a list containing all integers within a given range including from and to.
   */
  def range(from: Int, to: Int) = (for(i <- from to to) yield i).toList

  /**
   * P23 (**) Extract a given number of randomly selected elements from a list.
   */
  def randomSelect[T](noOfElem: Int, list: List[T]): List[T] = {
    import scala.util.Random._

    if(noOfElem <= 0) Nil
    else {
      val (rest, elem) = removeAt(nextInt(length(list)), list)
      elem :: randomSelect(noOfElem - 1, rest)
    }
  }

  /**
   * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   */
  def lotto(noOfElem: Int, M: Int): List[Int] = randomSelect(noOfElem, (for(i <- 1 to M) yield i).toList)

  /**
   * P25 (*) Generate a random permutation of the elements of a list.
   */
  def randomPermute[T](list: List[T]) = randomSelect(length(list), list)

  /**
   * P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
   */
  def combinations[T](k: Int, list: List[T]) = {

  }
}
