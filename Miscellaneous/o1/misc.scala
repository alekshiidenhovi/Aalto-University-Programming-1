package o1
object misc { // These definitions at the top are discussed in Chapter 5.2.

  // Various small assignments across several chapters will ask you to define functions in this file.
  // Please enter your code below this comment.

  def isPortrait(kuva: Pic) = {
    kuva.width < kuva.height
  }

  def isInOrder(pairOfNumbers: (Int, Int)) = pairOfNumbers._1 <= pairOfNumbers._2    // This example function is introduced in Chapter 8.4. You can ignore it until then.

  def together(bufferi: Vector[String], tempo: Int) = {
    bufferi.mkString("&") + "/" + tempo
  }

  def tempo(musiikki: String): Int = {
    val eka = musiikki.split("/")
    if (eka.size > 1) {
      val toka = eka.takeRight(1)
      toka(0).toInt
    } else {
      120
    }
  }
}

