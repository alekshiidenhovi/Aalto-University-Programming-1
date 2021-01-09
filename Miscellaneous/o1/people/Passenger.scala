package o1.people

class Passenger (val name: String, val card: Option[TravelCard]) {

  def hasCard: Boolean = this.card.isDefined

  def canTravel: Boolean = {
    card match {
      case None =>
        false
      case Some(kortti) =>
        if (kortti.isValid) true else false
    }
  }

}
