package o1

import constants._

class Game {
  val bug = new Bug(Pos(100, 40))
  private val suurin = new Obstacle(70)
  private val keskikokoinen = new Obstacle(30)
  private val pienin = new Obstacle(20)
  val obstacles = Vector[Obstacle](suurin, keskikokoinen, pienin)

  def timePasses() = {
    this.bug.fall()
    obstacles.foreach( _.approach() )
  }

  def activateBug() = {
    this.bug.flap(15)
  }

  def isLost = {
    if (obstacles.exists( _.touches(this.bug) ) || !this.bug.isInBounds) true else false
  }
  // Your code goes here. Please add only what is requested by the ebook. To avoid
  // confusing our automatic assessment system, please don't invent additions of your own
  // here (at least not until you're done with the ebook’s official FlappyBug assignments).

}




