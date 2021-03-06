package o1.legal


// TODO: define classes JuridicalPerson, HumanOrganization, GeographicalFeature, and Group.


abstract class JuridicalPerson(name: String) extends Entity(name)

abstract class HumanOrganization(name: String, val contact: NaturalPerson) extends JuridicalPerson(name)

class GeographicalFeature(name: String, val kind: String, val representative: Entity) extends JuridicalPerson(name) {
  def contact: NaturalPerson = this.representative.contact
}

class Group(val members: Vector[Entity]) extends JuridicalPerson("group") {
  val leader = members.head
  def contact: NaturalPerson = this.leader.contact
  def kind: String = "group of " + members.size + " led by " + leader.name
}

















// TODO: uncomment the classes below


class Nation(name: String, contact: NaturalPerson) extends HumanOrganization(name, contact) {
  def kind = "sovereign nation"

}

class Municipality(name: String, val nation: Nation, contact: NaturalPerson) extends HumanOrganization(name, contact) {
  def kind = "municipality of " + this.nation.name
}


class Corporation(val id: String, val seeksProfit: Boolean, name: String, contact: NaturalPerson) extends HumanOrganization(name, contact) {
  def kind = (if (this.seeksProfit) "for-" else "non-") + "profit corporation"
}


