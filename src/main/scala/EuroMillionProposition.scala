import scala.util.Random
import Helpers._

import scala.annotation.tailrec

class EuroMillionProposition(val numbers: List[Int], val stars: List[Int]) {
  override def toString: String = {
    val sortedNumbers = numbers.sorted.mkString(", ")
    val sortedStars = stars.sorted.mkString(", ")
    s"Numbers : $sortedNumbers | Stars : $sortedStars"
  }
}

object EuroMillionProposition {
  private val availableNumbers = (1 to 50).toList
  private val availableStars = (1 to 12).toList

  private def getRandomElement[A](list: List[A],
                                  random: Random = new Random): A =
    list(random.nextInt(list.size))

  /**
    * Returns a list of numbers picked randomly in the numbers list
    * @param numbers : a list of available number
    * @param random :  random object
    * @param size : the size of the returned list
    * @tparam A: Element of the list
    * @return List
    */
  private def getRandomCombination[A](numbers: List[A],
                                      random: Random = new Random,
                                      size: Int): List[A] = {
    // if the size is empty
    if (size <= 0) return Nil

    val randomNumber = getRandomElement(numbers, random)
    val newList = numbers.filter(number => number != randomNumber)

    randomNumber :: getRandomCombination(
      numbers = newList,
      size = size - 1,
      random = random
    )

  }

  def generateRandomPropositions(
    quantity: Int
  ): List[EuroMillionProposition] = {

    @tailrec
    def generateRandomPropositionsRec(
      accumulator: List[EuroMillionProposition] = Nil,
      quantity: Int
    ): List[EuroMillionProposition] = {
      if (quantity <= 0) return accumulator

      val randomCombinations = getRandomCombination(availableNumbers, size = 5)
      val randomStars = getRandomCombination(availableStars, size = 2)

      val randomProposition = new EuroMillionProposition(
        numbers = randomCombinations,
        stars = randomStars
      )

      generateRandomPropositionsRec(
        randomProposition :: accumulator,
        quantity - 1
      )
    }

    generateRandomPropositionsRec(Nil, quantity)

  }

  def getHistogram(propositions: List[EuroMillionProposition],
                   onStars: Boolean = false): List[(Int, Int)] =
    propositions.histogram(onStars)
}
