package o1

package object rainfall {

  def averageRainfall(vektori: Vector[Int]) = {
    val segmentti = vektori.takeWhile( _ != 999999 )
    val eiNegatiiviset = segmentti.filter( _ >= 0 )
    if (eiNegatiiviset.isEmpty)
      None
    else
      Some(eiNegatiiviset.sum / eiNegatiiviset.size)
  }

  def averageRainfallFromStrings(vektori: Vector[String]) = {
    val intiksi = vektori.map( _.toIntOption )
    val eiMerkkejä = intiksi.flatten
    averageRainfall(eiMerkkejä)
  }

  def drySpell(vektori: Vector[Int], length: Int) = {
    val kokoelma = vektori.sliding(length).toVector
    val filtteröity = kokoelma.indexWhere( vektor => vektor.forall( n => n >= 0 && n <= 5 ))
    filtteröity
  }







}