package o1.shapes
import scala.math._

class RightTriangle(x: Double, y: Double) extends Shape {

  def area = x * y / 2

  def hypotenuse = sqrt(x * x + y * y)

  def perimeter = x + y + this.hypotenuse

}
