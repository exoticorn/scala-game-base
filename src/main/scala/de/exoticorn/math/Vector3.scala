package de.exoticorn.math

final class Vector3(val x: Double, val y: Double, val z: Double) {
  def +(o: Vector3) = new Vector3(x + o.x, y + o.y, z + o.z)
  def -(o: Vector3) = new Vector3(x - o.x, y - o.y, z - o.z)
  def *(f: Double) = new Vector3(x * f, y * f, z * f)
  def *(o: Vector3) = x * o.x + y * o.y + z * o.z
  def /(f: Double) = this * (1 / f)

  def crossProduct(o: Vector3) =
    Vector3(
      y * o.z - z * o.y,
      z * o.x - x * o.z,
      x * o.y - y * o.x
      )
  
  def length = math.sqrt(x * x + y * y + z * z)
  def normalize = this / length

  override def equals(other: Any) = other match {
    case v: Vector3 => x == v.x && y == v.y && z == v.z
    case _ => false
  }
  override def hashCode() = 13 + x.hashCode() + y.hashCode() * 37 + z.hashCode() * 37*37
  override def toString = "Vector3(%f, %f, %f)".format(x, y, z)
}

object Vector3 {
  def apply(x: Double, y: Double, z: Double) = new Vector3(x, y, z)
  val xAxis = new Vector3(1, 0, 0)
  val yAxis = new Vector3(0, 1, 0)
  val zAxis = new Vector3(0, 0, 1)
}

