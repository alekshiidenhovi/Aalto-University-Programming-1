package o1.carsim
import o1.Pos


class Car(val fuelConsumption: Double, val tankSize: Double, initialFuel: Double, initialLocation: Pos) {

  private var currentLocation = initialLocation

  private var currentFuel = initialFuel

  private var ajettuMatka = 0.0

  def location = this.currentLocation

  def fuel(toBeAdded: Double): Double = {
    if (tankSize >= currentFuel + toBeAdded) {
      currentFuel += toBeAdded
      toBeAdded
    } else {
      val bensa = tankSize - currentFuel
      currentFuel = tankSize
      bensa
    }
  }

  def fuel(): Double = {
    val bensa = tankSize - currentFuel
    currentFuel = tankSize
    bensa
  }

  def fuelRatio: Double = currentFuel / tankSize * 100

  def metersDriven: Double = ajettuMatka

  def fuelRange: Double = currentFuel / fuelConsumption * 100000

  def drive(destination: Pos, metersToDestination: Double): Unit = {
    if (currentFuel >= fuelConsumption * metersToDestination / 100000) {
      ajettuMatka += metersToDestination
      currentLocation = destination
      currentFuel -= fuelConsumption * metersToDestination / 100000
    } else {
      ajettuMatka += this.fuelRange
      currentLocation = currentLocation.addX((this.fuelRange / metersToDestination) * (destination.x - currentLocation.x))
      currentLocation = currentLocation.addY((this.fuelRange / metersToDestination) * (destination.y - currentLocation.y))
      currentFuel = 0

    }
  }
}
