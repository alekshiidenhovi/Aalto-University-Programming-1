
package o1.shapes

// This trait is introduced in Chapter 7.2.

trait Shape {

  def isBiggerThan(another: Shape) = this.area > another.area

  def area: Double

  def perimeter: Double

}

