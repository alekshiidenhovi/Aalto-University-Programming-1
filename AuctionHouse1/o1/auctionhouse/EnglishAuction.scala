package o1.auctionhouse

import scala.math._

class EnglishAuction(val description: String, val startingPrice: Int, duration: Int) {

  // Keep these variables; they will be useful for implementing the methods.
  private var highest = new Bid(None, startingPrice)       // most-wanted holder
  private var secondHighest = new Bid(None, startingPrice) // most-wanted holder
  private var remainingDays = duration


  // However, the method implementations given below leave a lot to be desired.


  def daysLeft: Int = this.remainingDays


  def hasNoBids: Boolean = this.highest.isInitialBid


  def advanceOneDay() = {
    if (this.isOpen) {
      this.remainingDays -= 1
    }
  }


  def isOpen: Boolean = this.remainingDays > 0


  def isExpired: Boolean = !this.isOpen && this.hasNoBids


  def buyer: Option[String] = this.highest.bidder


  def price: Int = {
    if (this.secondHighest.isInitialBid)
      this.startingPrice
    else
      min(this.secondHighest.limit + 1, this.highest.limit)
  }

  def requiredBid: Int = if (this.hasNoBids) this.startingPrice else this.price + 1


  def bid(bidder: String, amount: Int): Boolean = {
    val newBid = new Bid(Some(bidder), amount)
    if (this.isOpen && amount >= this.requiredBid) {
      this.secondHighest = if (newBid.beats(this.highest)) this.highest else newBid.winner(this.secondHighest)
      this.highest = newBid.winner(this.highest)
    }
    this.highest == newBid
  }


  override def toString = this.description


}
