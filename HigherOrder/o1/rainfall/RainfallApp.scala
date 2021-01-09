package o1.rainfall

import scala.io.StdIn._

object RainfallApp extends App {

  def userInputs = LazyList.continually(readLine("Enter rainfall (or 999999 to stop): "))
  def cut = userInputs.takeWhile( _.toInt != 999999 ).toVector
  averageRainfallFromStrings(cut) match {
    case Some(content) =>
      println("The average is " + content)
    case None =>
      println("No valid data. Cannot compute average.")
  }

  // Enter your program here as instructed in Chapter 7.1.

}
