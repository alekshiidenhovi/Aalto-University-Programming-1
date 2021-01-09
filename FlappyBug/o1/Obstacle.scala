package o1

import constants._

import scala.util.Random


// This class is introduced in Chapter 2.6.

class Obstacle(val radius: Int) {

  private var currentPos = this.randomLaunchPosition()

  def pos = this.currentPos

  def isActive = this.pos.x >= -this.radius

  def randomLaunchPosition() = {
    val launchX = ViewWidth + this.radius + Random.nextInt(500)
    val launchY = Random.nextInt(400)
    new Pos(launchX, launchY)
  }

  def approach() = {
    if (this.isActive) {
      this.currentPos = this.currentPos.addX(-ObstacleSpeed)
    } else {
      this.currentPos = this.randomLaunchPosition
    }
  }

  def touches(bug: Bug) = {
    this.radius + bug.radius >= bug.pos.distance(this.pos)
  }

  override def toString = "center at " + this.pos + ", radius " + this.radius

}
