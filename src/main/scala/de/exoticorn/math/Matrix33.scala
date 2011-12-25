package de.exoticorn.math

final class Matrix33(val x: Vector3, val y: Vector3, val z: Vector3) {
  def *(o: Vector3): Vector3 = x * o.x + y * o.y + z * o.z
  def *(o: Matrix33): Matrix33 = new Matrix33(this * o.x, this * o.y, this * o.z)
  def *(f: Double) = new Matrix33(x * f, y * f, z * f)
  
  def transpose =
    Matrix33(
      Vector3(x.x, y.x, z.x),
      Vector3(x.y, y.y, z.y),
      Vector3(x.z, y.z, z.z))

  def inverse = {
    val det = x.x * y.y * z.z + y.x * z.y * x.z + z.x * x.y * y.z -
      x.x * y.z * z.y - y.x * z.z * x.y - z.x * x.z * y.y
    val cof = new Matrix33(
      Vector3(y.y * z.z - y.z * z.y, -(y.x * z.z - y.z * z.x), y.x * z.y - y.y * z.x),
      Vector3(-(x.y * z.z - x.z * z.y), x.x * z.z - x.z * z.x, -(x.x * z.y - x.y * z.x)),
      Vector3(x.y * y.z - x.z * y.y, -(x.x * y.z - x.z * y.x), x.x * y.y - x.y * y.x))
    cof.transpose * (1 / det)
  }

  override def equals(other: Any) = other match {
    case m: Matrix33 => x == m.x && y == m.y && z == m.z
    case _ => false
  }
  override def hashCode() = 13 + x.hashCode() + y.hashCode() * 41 + z.hashCode() * 41*41
  override def toString = "Matrix33(%s, %s, %s)".format(x, y, z)
}

object Matrix33 {
  def apply(x: Vector3, y: Vector3, z: Vector3) = new Matrix33(x, y, z)
  val unit = new Matrix33(Vector3.xAxis, Vector3.yAxis, Vector3.zAxis)
  def rotX(angle: Double) = {
    val s = math.sin(angle)
    val c = math.cos(angle)
    new Matrix33(Vector3.xAxis, Vector3(0, c, -s), Vector3(0, s, c))
  }
  def rotY(angle: Double) = {
    val s = math.sin(angle)
    val c = math.cos(angle)
    new Matrix33(Vector3(c, 0, s), Vector3.yAxis, Vector3(-s, 0, c))
  }
  def rotZ(angle: Double) = {
    val s = math.sin(angle)
    val c = math.cos(angle)
    new Matrix33(Vector3(c, -s, 0), Vector3(s, c, 0), Vector3.zAxis)
  }
}

