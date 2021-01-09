package o1.auctionhouse
import scala.math._

class FixedPriceSale(val description: String, val price: Int, duration: Int) {

  private var ostajanNimi: Option[String] = None
  private var aukioloaika = this.duration

  override def toString = description

  def daysLeft: Int = max(aukioloaika, 0)

  def buyer: Option[String] = ostajanNimi

  def isExpired = {
    if (ostajanNimi.isDefined)
      false
    else if (ostajanNimi.isEmpty && aukioloaika > 0)
      false
    else
      true
  }

  def isOpen = {
    if (ostajanNimi.isEmpty && aukioloaika > 0) true else false
  }

  def advanceOneDay() = {
    if (ostajanNimi.isEmpty)
      aukioloaika -= 1
  }

  def buy(buyer: String) = {
    if (aukioloaika > 0)
      ostajanNimi match {
        case None =>
          ostajanNimi = Some(buyer)
          true
        case Some(ostaja) =>
          false
      }
    else
      false
  }
}
