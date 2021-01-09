package o1.football3
import java.awt.geom
import java.awt.geom.NoninvertibleTransformException
import scala.math._
import scala.collection.mutable.Buffer

class Season {
  private val ottelut = Buffer[Match]()
  private var suurinEro: Option[Match] = None

  def addResult(newResult: Match) = {
    this.ottelut += newResult
    this.suurinEro match {
      case None =>
        this.suurinEro = Some(newResult)
      case Some(vanha) =>
        if (vanha.goalDifference == 0)
          if(newResult.goalDifference != 0)
            this.suurinEro = Some(newResult)
          else this.suurinEro = Some(vanha)
        else if (vanha.goalDifference != 0)
          if (abs(vanha.goalDifference) < abs(newResult.goalDifference))
            this.suurinEro = Some(newResult)
    }
  }

  def biggestWin: Option[Match] = {
    if (this.ottelut.isEmpty)
      None
    else
      this.suurinEro
  }

  def latestMatch: Option[Match] = {
    if (ottelut.nonEmpty)
      this.ottelut.toVector.lastOption
    else
      None
  }

  def matchNumber(number: Int): Option[Match] = {
    if (ottelut.toVector.nonEmpty)
      ottelut.toVector.lift(number)
    else
      None
  }

  def numberOfMatches: Int = this.ottelut.toVector.size

}

