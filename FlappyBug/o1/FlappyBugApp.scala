package o1

import constants._

// This class is introduced in Chapter 2.7.

object FlappyBugApp extends App {

  val sky        = rectangle(ViewWidth, ViewHeight,  LightBlue)
  val ground     = rectangle(ViewWidth, GroundDepth, SandyBrown)
  val trunk      = rectangle(30, 250, SaddleBrown)
  val foliage    = circle(200, ForestGreen)
  val tree       = trunk.onto(foliage, TopCenter, Center)
  val rootedTree = tree.onto(ground, BottomCenter, new Pos(ViewWidth / 2, 30))
  val scenery    = sky.place(rootedTree, BottomLeft, BottomLeft)


  val bugPic = Pic("ladybug.png")


  def rockPic(obstacle: Obstacle) = circle(obstacle.radius * 2, Black)


  // INSERT YOUR OWN CODE BELOW.

  val peli = new Game
  val gui = new View(peli, "FlappyBug") {
    var background = scenery
    def makePic = {
      var kuva = background
      kuva = peli.obstacles.foldLeft(kuva.place(bugPic, peli.bug.pos))( (result, n) => result.place(rockPic(n), n.pos) )
      kuva

  }






    override def isDone =  peli.isLost

    override def onKeyDown(painettu: Key) = {
      if (painettu == Key.Space) {
        peli.activateBug()
      }
    }

    override def onTick() = {
      peli.timePasses()
      this.background = this.background.shiftLeft(FallingSpeed)
    }
  }

  gui.start()







}

