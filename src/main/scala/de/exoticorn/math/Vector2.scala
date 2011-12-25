package de.exoticorn.math

final class Vector2(val x: Double, val y: Double) {
  def +(o: Vector2) = new Vector2(x + o.x, y + o.y)
  def -(o: Vector2) = new Vector2(x - o.x, y - o.y)
  def *(f: Double) = new Vector2(x * f, y * f)
  def *(o: Vector2) = x * o.x + y * o.y
  def /(f: Double) = this * (1 / f)

  def length = math.sqrt(x * x + y * y)
  def normalize = this / length

  def angle = math.atan2(y, x)

  override def equals(other: Any) = other match {
    case v: Vector2 => x == v.x && y ==v.y
    case _ => false
  }
  override def hashCode() = 13 + x.hashCode() + y.hashCode() * 37
  override def toString = "Vector2(%f, %f)".format(x, y)
}

object Vector2 {
  def apply(x: Double, y: Double) = new Vector2(x, y)
  def fromAngle(angle: Double, length: Double = 1) = new Vector2(math.cos(angle) * length, math.sin(angle) * length)
  val xAxis = new Vector2(1, 0)
  val yAxis = new Vector2(0, 1)
}
  
