object Main extends App {
  val propositions = EuroMillionProposition.generateRandomPropositions(4)

  propositions.foreach(println)
}
