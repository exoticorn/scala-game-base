package de.exoticorn.math

class Matrix32(val rot: Matrix22, val pos: Vector2) {
  def *(o: Vector2): Vector2 = rot * o + pos
  def *(o: Matrix32): Matrix32 = new Matrix32(rot * o.rot, this * o.pos)

  def inverse: Matrix32 = {
    val invRot = rot.inverse
    new Matrix32(invRot, invRot * pos * -1)
  }

  override def equals(other: Any) = other match {
    case m: Matrix32 => rot == m.rot && pos == m.pos
    case _ => false
  }
  override def hashCode = rot.hashCode + pos.hashCode * 97

  override def toString = "Matrix32(%s, %s, %s)".format(rot.x, rot.y, pos)
}

object Matrix32 {
  def apply(rot: Matrix22, pos: Vector2) = new Matrix32(rot, pos)
  def apply(x: Vector2, y: Vector2, pos: Vector2) = new Matrix32(new Matrix22(x, y), pos)
  val unit = new Matrix32(Matrix22.unit, Vector2(0, 0))
}

