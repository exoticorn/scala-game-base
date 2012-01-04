package de.exoticorn.math

class Matrix22(val x: Vector2, val y: Vector2) {
  def *(o: Vector2): Vector2 = x * o.x + y * o.y
  def *(o: Matrix22): Matrix22 = new Matrix22(this * o.x, this * o.y)
  def *(f: Double) = new Matrix22(x * f, y * f)

  def transpose: Matrix22 = new Matrix22(
    new Vector2(x.x, y.x),
    new Vector2(x.y, y.y))

  def inverse: Matrix22 = {
    val det = x.x * y.y - y.x * x.y
    val cof = new Matrix22(
      Vector2(y.y, -y.x),
      Vector2(-x.y, x.x))
    cof.transpose * (1 / det)
  }
  
  override def equals(other: Any) = other match {
    case m: Matrix22 => x == m.x && y == m.y
    case _ => false
  }
  override def hashCode = x.hashCode + y.hashCode * 71

  override def toString = "Matrix22(%s, %s)".format(x, y)
}

object Matrix22 {
  def apply(x: Vector2, y: Vector2) = new Matrix22(x, y)
  val unit = new Matrix22(Vector2(1, 0), Vector2(0, 1))
}