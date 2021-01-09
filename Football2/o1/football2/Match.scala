package o1.football2

import scala.collection.mutable.Buffer


/** The class `Match` represents match results in a football match statistics program.
  * A match is played between teams from two clubs: a home club and an away club.
  * Goals scored by players of either team can be added to the match object with the
  * method `addGoal`.
  *
  * The class is expected to be used so that a match object with no goals is initially
  * created as a real-life match starts. Goals are added incrementally as the match
  * progresses. (A match object has mutable state.)
  *
  * @param home  the club whose team plays at home in the match
  * @param away  the club whose team plays away in the match */
class Match(val home: Club, val away: Club) {

  private val homeScorers = Buffer[Player]()    // container: goalscorers of the home team are added here
  private val awayScorers = Buffer[Player]()    // container: goalscorers of the away team are added here


  def winnerName = {
    if (isAwayWin)
      this.away.name
    else if (isHomeWin)
      this.home.name
    else
      "no winner"
  }

  def winningScorerName: String = {
    if (isAwayWin) {
      this.awayScorers.apply(this.homeGoals).name
    } else if (isHomeWin) {
      this.homeScorers.apply(this.awayGoals).name
    } else {
      "no winning goal"
    }
  }

  def homeGoals: Int = this.homeScorers.size
  def awayGoals: Int = this.awayScorers.size
  def totalGoals: Int = this.homeGoals + this.awayGoals
  def goalDifference: Int = this.homeGoals - this.awayGoals

  def isAwayWin: Boolean = this.goalDifference < 0
  def isHomeWin: Boolean = this.goalDifference > 0
  def isTied: Boolean = this.goalDifference == 0
  def isGoalless: Boolean = this.totalGoals == 0
  def isHigherScoringThan(anotherMatch: Match) = this.totalGoals > anotherMatch.totalGoals

  def allScorers: Vector[Player] = this.homeScorers.toVector ++ this.awayScorers.toVector

  def hasScorer(possibleScorer: Player): Boolean = {
    if (this.allScorers.contains(possibleScorer)) true else false
  }

  def addGoal(scorer: Player): Unit = {
    if (scorer.employer == this.home) {
      this.homeScorers += scorer
    } else if (scorer.employer == this.away)
      this.awayScorers += scorer
  }

  def location = home.stadium

  override def toString = {
    if (this.isGoalless) {
      "" + home.name + " vs. " + away.name + " at " + home.stadium + ": tied at nil-nil"
    } else if (this.isTied) {
      "" + home.name + " vs. " + away.name + " at " + home.stadium + ": tied at " + this.totalGoals / 2 + "-all"
    } else if (this.isHomeWin) {
      "" + home.name + " vs. " + away.name + " at " + home.stadium + ": " + this.homeGoals + "-" + this.awayGoals + " to " + home.name
    } else {
      "" + home.name + " vs. " + away.name + " at " + home.stadium + ": " + this.awayGoals + "-" + this.homeGoals + " to " + away.name
    }
  }




}


