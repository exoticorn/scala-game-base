package de.exoticorn.math

final class Plane(val normal: Vector3, val t: Double) {
  def height(pos: Vector3) = normal * pos - t

  override def equals(other: Any) = other match {
    case p: Plane => normal == p.normal && t == p.t
    case _ => false
  }
  override def hashCode() = 13 + normal.hashCode() + t.hashCode() * 71
  override def toString = "Plane(%s, %f)".format(normal, t)
}

object Plane {
  def apply(normal: Vector3, t: Double) = new Plane(normal, t)
  def apply(v1: Vector3, v2: Vector3, v3: Vector3) = {
    val normal = (v3 - v1).crossProduct(v2 - v1).normalize
    new Plane(normal, normal * v1)
  }
}

