package o1.election

import scala.collection.mutable.Buffer           // This is useful in early versions of the class.
import scala.math.Ordering.Double.TotalOrdering  // This will be useful in later assignments.

// Write your code here.

class District(val name: String, val seats: Int, val candidates: Vector[Candidate]) {

  def candidatesFrom(party: String): Vector[Candidate] = {
    candidates.filter( _.party == party )
  }

  def printCandidates(): Unit = {
    candidates.foreach(n => println(n))
  }

  override def toString = s"$name: " + candidates.size + s" candidates, $seats seats"

  def topCandidate: Candidate = {
    var tämänHetkinen = this.candidates.head
    for (current <- candidates) {
      if (current.>(tämänHetkinen))
        tämänHetkinen = current
    }
    tämänHetkinen
  }

  def totalVotes(party: String): Int = {
    val puolue = this.candidatesFrom(party)
    puolue.foldLeft(0)( _ + _.votes )
  }

  def totalVotes: Int = {
    val kandidaatit = candidates.map( _.votes )
    kandidaatit.sum
  }

}