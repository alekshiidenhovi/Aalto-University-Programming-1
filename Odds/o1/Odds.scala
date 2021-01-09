package o1

// This class is gradually developed between Chapters 2.4 and 3.4.

class Odds(val wont: Int, val will: Int) {

  def probability = 1.0 * this.will / (this.wont + this.will)
  def fractional =  "" + this.wont + "/" + this.will
  override def toString = fractional
  def decimal = 1.0 / (1.0 * this.will / (this.wont + this.will))
  def winnings(luku: Double) = luku * (1.0 / (1.0 * this.will / (this.wont + this.will)))
  def not = new Odds(will, wont)
  def both(toinen: Odds) = new Odds(this.wont * toinen.wont + this.wont * toinen.will + this.will * toinen.wont, this.will * toinen.will)
  def either(toinen: Odds) = new Odds(this.wont * toinen.wont, this.wont * toinen.will + this.will * toinen.wont + this.will * toinen.will)
  def isLikely = this.will > this.wont
  def isLikelierThan(another: Odds) = this.probability > another.probability
  def moneyline = {
    if (this.will <= this.wont)
      100 * this.wont / this.will
    else
      -100 * this.will / this.wont
  }
  // TODO: other methods missing

}
