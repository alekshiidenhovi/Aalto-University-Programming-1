package o1.blood

// This code is introduced in Chapter 7.2.

trait Rhesus {
  val isPositive: Boolean
  def isNegative = !this.isPositive
  def canDonateTo(recipient: Rhesus) = this.isNegative || this == recipient
  def canReceiveFrom(donor: Rhesus) = donor.canDonateTo(this)
}

trait ABO {

  val antigens: String

  def canDonateTo(recipient: ABO) = {
    if (this.antigens == "")
      true
    else if (this.antigens == "A" && (recipient.antigens == "A" || recipient.antigens == "AB"))
      true
    else if (this.antigens == "B" && (recipient.antigens == "B" || recipient.antigens == "AB"))
      true
    else if (this.antigens == "AB" && recipient.antigens == "AB")
      true
    else
      false
  }

  def canReceiveFrom(donor: ABO) = {
    if (this.antigens == "AB")
      true
    else if (this.antigens == "A" && (donor.antigens == "A" || donor.antigens == ""))
      true
    else if (this.antigens == "B" && (donor.antigens == "B" || donor.antigens == ""))
      true
    else if (this.antigens == "" && donor.antigens == "")
      true
    else
      false
  }

}

object RhPlus extends Rhesus {
  val isPositive = true
  override def toString = "+"
}

object RhMinus extends Rhesus {
  val isPositive = false
  override def toString = "-"
}

object A extends ABO {
  val antigens = "A"
  override def toString = antigens
}

object B extends ABO {
  val antigens = "B"
  override def toString = antigens
}

object AB extends ABO {
  val antigens = "AB"
  override def toString = antigens
}

object O extends ABO {
  val antigens = ""
  override def toString = "O"
}

// You can write your ABO trait and the four singleton objects here.





















// You don't need to edit the ABORh class below. See the ebook chapter for more info.
/*
class ABORh(val abo: ABO, val rhesus: Rhesus) {
  def canDonateTo(recipient: ABORh) = this.abo.canDonateTo(recipient.abo) && this.rhesus.canDonateTo(recipient.rhesus)
  def canReceiveFrom(donor: ABORh) = donor.canDonateTo(this)
  override def toString = this.abo.toString + this.rhesus.toString
}
*/
