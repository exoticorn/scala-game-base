package de.exoticorn.math

final class Vector4(val x: Double, val y: Double, val z: Double, val w: Double) {
  def +(o: Vector4) = new Vector4(x + o.x, y + o.y, z + o.z, w + o.w)
  def -(o: Vector4) = new Vector4(x - o.x, y - o.y, z - o.z, w - o.w)
  def *(f: Double) = new Vector4(x * f, y * f, z * f, w * f)
  def *(o: Vector4) = x * o.x + y * o.y + z * o.z + w * o.w
  def /(f: Double) = this * (1 / f)

  def length = math.sqrt(x*x + y*y + z*z + w*w)
  def normalize = this / length
  
  override def equals(other: Any) = other match {
    case v: Vector4 => x == v.x && y == v.y && z == v.z && w == v.w
    case _ => false
  }
  override def hashCode() = 13 + x.hashCode() + y.hashCode() * 37 + z.hashCode() * 37*37 + w.hashCode() * 37*37*37
  override def toString = "Vector4(%f, %f, %f, %f)".format(x, y, z, w)
}

object Vector4 {
  def apply(x: Double, y: Double, z: Double, w: Double) = new Vector4(x, y, z, w)
  val xAxis = new Vector4(1, 0, 0, 0)
  val yAxis = new Vector4(0, 1, 0, 0)
  val zAxis = new Vector4(0, 0, 1, 0)
  val wAxis = new Vector4(0, 0, 0, 1)
}

