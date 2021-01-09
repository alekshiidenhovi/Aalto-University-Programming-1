import scala.io.StdIn.readLine

object Ymparoi extends App {
  val eka = readLine("Anna ympäröitävät merkit: ")
  val toka = readLine("Anna ympäröivät merkit: ")
  println(toka + eka + toka)
}