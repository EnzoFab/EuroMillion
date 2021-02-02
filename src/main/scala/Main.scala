import scala.io.StdIn.readLine
import Helpers._

import scala.annotation.tailrec

object Main extends App {

  @tailrec
  def run(): Unit = {
    println(
      "What do you want to do ?\n\t1- Generate random proposition\n\t2- Generate histogram\n\t3- Exit\n"
    )
    val answer = readLine().toInteger().getOrElse(3)

    answer match {
      case 1 => generateProposition()
      case 2 => displayHistogram()
      case _ =>
        println("Goodbye !")
        return
    }

    println("Continue ?\n1- Yes\n2- No")
    val exitAnswer = readLine().toInteger().getOrElse(2)

    if (exitAnswer == 1) {
      run()
    }

  }

  def generateProposition(): Unit = {
    println("How many proposition do you want to generate ?\n")
    val answer: String = readLine()

    val nbPropositions: Int = answer.toInteger().getOrElse(-1)

    if (nbPropositions <= 0) {
      println("Answer format not handled, retry ?\n1- Yes\n2- No")
      val newAnswer = readLine()

      if (newAnswer.toInteger().getOrElse(2) == 1)
        generateProposition()
      else return
    }

    val propositions =
      EuroMillionProposition.generateRandomPropositions(nbPropositions)

    println("\nThe generator gave you the following proposition(s)\n")
    propositions.foreach(println)

  }

  def displayHistogram(): Unit = {
    println("What is the size of the histogram ?")
    val size: Int = readLine().toInteger().getOrElse(0)

    if (size <= 0) {
      println("Wrong answer ! As a reminder the size must be greater than 0")
      return
    }

    val prop_hist = EuroMillionProposition.generateRandomPropositions(size)

    val histogram =
      EuroMillionProposition.getHistogram(prop_hist)

    val stars =
      EuroMillionProposition.getHistogram(prop_hist, onStars = true)

    println("\nStats:")
    histogram.foreach(println)

    println("\nOn starts:")
    stars.foreach(println)
  }

  run()
}
