package o1

import constants._

// Define class Bug here.



class Bug(private var nytPos: Pos) {
  def pos = this.nytPos
  override def toString = "center at " + this.nytPos + ", radius 15"
  val radius = 15
  private var yVelocity = 0.0

  def flap(ylös: Double) = {
    yVelocity = -1.0 * ylös
  }

  def fall() = {
    if (this.pos.y != 350) {
      yVelocity = yVelocity + FallingSpeed
    }
      move(yVelocity)
  }

  def move(muutos: Double) = {
    this.nytPos = this.nytPos.addY(muutos).clampY(0, 350)
  }

  def isInBounds: Boolean = this.pos.y > 0 && this.pos.y < 350

}

