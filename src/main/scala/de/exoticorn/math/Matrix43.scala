package de.exoticorn.math

final class Matrix43(val rot: Matrix33, val pos: Vector3) {
  def *(o: Vector3): Vector3 = rot * o + pos
  def *(o: Matrix43): Matrix43 = new Matrix43(rot * o.rot, rot * o.pos + pos)
  def inverse = {
    val inv = rot.inverse
    new Matrix43(inv, inv * pos * -1)
  }

  override def equals(other: Any) = other match {
    case m: Matrix43 => rot == m.rot && pos == m.pos
    case _ => false
  }
  override def hashCode() = 13 + rot.hashCode() + pos.hashCode() * 71
  override def toString = "Matrix43(%s, %s, %s, %s)".format(rot.x, rot.y, rot.z, pos)
}

object Matrix43 {
  def apply(rot: Matrix33, pos: Vector3) = new Matrix43(rot, pos)
  def apply(x: Vector3, y: Vector3, z: Vector3, pos: Vector3) = new Matrix43(new Matrix33(x, y, z), pos)
  val unit = new Matrix43(Matrix33.unit, Vector3(0, 0, 0))
}

