package o1.people

class Member(val id: Int, val name: String, val yearOfBirth: Int, val yearOfDeath: Option[Int]) {

  def isAlive: Boolean = yearOfDeath == None

  override def toString = {
    yearOfDeath match {
      case None =>
        "" + name + "(" + yearOfBirth + "-)"
      case Some(kuollut) =>
        "" + name + "(" + yearOfBirth + "-" + kuollut + ")"
    }
  }

}
