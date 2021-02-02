object Helpers {

  /**
    * Implicit classes allow to add methods to an existing object
    * Here we add a method histogram to object List[EuroMillionProposition]
    * @param list
    * @see EuroMillionProposition
    */
  implicit class Aggregate(list: List[EuroMillionProposition]) {

    /**
      * Count the number of occurrences of each element of the combinations
      * @param onStars
      * @tparam A
      * @return a list of tuple
      */
    def histogram[A](onStars: Boolean = false): List[(Int, Int)] = {
      val combinations =
        if (onStars) list.flatMap(e => e.stars)
        else list.flatMap(e => e.numbers)

      // result: Map((number1 -> nbOccurences), (number1 -> nbOccurences))
      val aggregate = combinations.groupBy(identity).map(t => (t._1, t._2.size))

      val listAggregate = aggregate.toList

      // sort by the number of occurrence
      listAggregate.sortWith(_._2 > _._2)

    }
  }

  implicit class IntegerCast(s: String) {

    /**
      * Try to cast a string to int
      * @param s: string
      * @return
      */
    def toInteger(): Option[Int] = {
      try {
        Some(s.toInt)
      } catch {
        case e: Exception => None
      }
    }
  }

}
