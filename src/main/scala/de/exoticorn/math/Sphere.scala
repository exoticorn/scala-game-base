package de.exoticorn.math

final case class Sphere(center: Vector3, radius: Double) {
  def |(point: Vector3) = {
    val toPoint = point - center
    val distance = toPoint.length
    if(distance <= radius)
      this
    else {
      val growBy = (distance - radius) / 2
      val newCenter = center + toPoint * (growBy / distance)
      val newRadius = radius + growBy
      new Sphere(newCenter, newRadius)
    }
  }
}
