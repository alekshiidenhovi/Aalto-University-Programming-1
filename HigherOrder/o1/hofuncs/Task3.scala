package o1.hofuncs

// This program is introduced in Chapter 6.1.

import o1._

object Task3 extends App {

  // This task is similar to the previous one: you create an image filter for
  // unscrambling another hidden image. Here is what you need to know this time:
  //
  // * This image, too, contains some randomly generated noise.
  //   The values of the red and green component are meaningless.
  //
  // * Some but not all values of the blue component are important:
  //
  //      * Values between 0 and 15 are meaningful. However,
  //        they are rather too dim. Multiply them by sixteen
  //        to obtain the appropriate degree of blueness.
  //      * All other values (16-255) of the blue component are
  //        meaningless and should be replaced by zero.
  //
  // * The hidden image is in grayscale, so every pixel's R, G, and B components
  //   are equal. Assign the amplified blueness to each component.

  def reveal(väri: Color) = {
    var sininen = väri.blue
    if (väri.blue >= 0 && väri.blue <= 15) {
      sininen = väri.blue * 16
    } else if (väri.blue >= 16 && väri.blue <= 255) {
      sininen = 0
    }
    Color(sininen, sininen, sininen)
  }

  val scrambledPic = Pic("hidden2.png")
  val solvedPic = scrambledPic.transformColors(reveal)
  launchUnscramblerGUI(scrambledPic, solvedPic)

}
