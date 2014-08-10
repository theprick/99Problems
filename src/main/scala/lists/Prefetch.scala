/**
 * Iterate through a list, call getPartialResult for each item, then aggregate the result in a single Map
 */
object Prefetch {
  def main(args : Array[String]) {
    print(getResult(List(1, 2, 3)))
  }

  /**
   * @return some map with some Strings
   */
  def getPartialResult(id: Int) = {
    id match {
      case 1 => Map("11" -> "some value", "12" -> "some value", "13" -> "some value")
      case 2 => Map("21" -> "some value", "22" -> "some value", "23" -> "some value")
      case 3 => Map("31" -> "some value", "32" -> "some value")
      case _ => Map("0" -> "default")
    }
  }

  /**
   * Some explanations: "++" means putAll, the accumulator can be immutable.
   * @return the aggregated results
   */
  def getResult(ids: List[Int]) = ids.foldLeft(Map[String, String]())((acc, id ) => acc ++ getPartialResult(id))
} 