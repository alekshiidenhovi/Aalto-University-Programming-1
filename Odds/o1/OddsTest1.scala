package o1

// This program is developed in Chapters 2.7 and 3.4.
// It creates a single Odds object and uses some of its methods.

import scala.io.StdIn._

object OddsTest1 extends App {

  println("Please enter the odds of an event as two integers on separate lines.")
  println("For instance, to enter the odds 5/1 (one in six chance of happening), write 5 and 1 on separate lines.")
  val firstInput = readInt()
  val secondInput = readInt()

  val uusi = new Odds(firstInput, secondInput)
  println("The odds you entered are:")
  println("In fractional format: " + uusi.fractional)
  println("In decimal format: " + uusi.decimal)
  println("In moneyline format: " + uusi.moneyline)
  println("Event probability: " + uusi.probability)
  println("Reverse odds: " + uusi.not.fractional)
  println("Odds of happening twice: " + uusi.both(uusi))

  println("Please enter the size of a bet:")
  val thirdInput = readDouble()
  println("If successful, the bettor would claim " + uusi.winnings(thirdInput))
  println("Please enter the odds of a second event as two integers on separate lines.")
  val fourthInput = readInt()
  val fifthInput = readInt()

  val toinen = new Odds(fourthInput, fifthInput)
  println("The odds of both events happening are: " + toinen.both(uusi))
  println("The odds of one or both happening are: " + toinen.either(uusi))

}

