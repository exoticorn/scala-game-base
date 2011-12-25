package de.exoticorn.math

final class Matrix44(val x: Vector4, val y: Vector4, val z: Vector4, val w: Vector4) {
  def *(o: Vector4): Vector4 = x * o.x + y * o.y + z * o.z + w * o.w
  def *(o: Matrix44): Matrix44 = new Matrix44(this * o.x, this * o.y, this * o.z, this * o.w)
  def *(o: Matrix43): Matrix44 = this * Matrix44(o)
  def *(f: Double) = new Matrix44(x * f, y * f, z * f, w * f)

  def transpose =
    new Matrix44(
      Vector4(x.x, y.x, z.x, w.x),
      Vector4(x.y, y.y, z.y, w.y),
      Vector4(x.z, y.z, z.z, w.z),
      Vector4(x.w, y.w, z.w, w.w))

  def inverse = {
    val det = x.x * y.y * z.z * w.w + x.x * z.y * w.z * y.w + x.x * w.y * y.z * z.w +
    y.x * x.y * w.z * z.w + y.x * z.y * x.z * w.w + y.x * w.y * z.z * x.w +
    z.x * x.y * y.z * w.w + z.x * y.y * w.z * x.w + z.x * w.y * x.z * y.w +
    w.x * x.y * z.z * y.w + w.x * y.y * x.z * z.w + w.x * z.y * y.z * x.w -
    x.x * y.y * w.z * z.w - x.x * z.y * y.z * w.w - x.x * w.y * z.z * y.w -
    y.x * x.y * z.z * w.w - y.x * z.y * w.z * x.w - y.x * w.y * x.z * z.w -
    z.x * x.y * w.z * y.w - z.x * y.y * x.z * w.w - z.x * w.y * y.z * x.w -
    w.x * x.y * y.z * z.w - w.x * y.y * z.z * x.w - w.x * z.y * x.z * y.w

    val inv_x_x = y.y * z.z * w.w + z.y * w.z * y.w + w.y * y.z * z.w -
      y.y * w.z * z.w - z.y * y.z * w.w - w.y * z.z * y.w
    val inv_y_x = y.x * z.z * w.w + z.x * w.z * y.w + w.x * y.z * z.w -
      y.x * w.z * z.w - z.x * y.z * w.w - w.x * z.z * y.w
    val inv_z_x = y.x * z.y * w.w + z.x * w.y * y.w + w.x * y.y * z.w -
      y.x * w.y * z.w - z.x * y.y * w.w - w.x * z.y * y.w
    val inv_w_x = y.x * z.y * w.z + z.x * w.y * y.z + w.x * y.y * z.z -
      y.x * w.y * z.z - z.x * y.y * w.z - w.x * z.y * y.z
    val inv_x_y = x.y * z.z * w.w + z.y * w.z * x.w + w.y * x.z * z.w -
      x.y * w.z * z.w - z.y * x.z * w.w - w.y * z.z * x.w
    val inv_y_y = x.x * z.z * w.w + z.x * w.z * x.w + w.x * x.z * z.w -
      x.x * w.z * z.w - z.x * x.z * w.w - w.x * z.z * x.w
    val inv_z_y = x.x * z.y * w.w + z.x * w.y * x.w + w.x * x.y * z.w -
      x.x * w.y * z.w - z.x * x.y * w.w - w.x * z.y * x.w
    val inv_w_y = x.x * z.y * w.z + z.x * w.y * x.z + w.x * x.y * z.z -
      x.x * w.y * z.z - z.x * x.y * w.z - w.x * z.y * x.z
    val inv_x_z = x.y * y.z * w.w + y.y * w.z * x.w + w.y * x.z * y.w -
      x.y * w.z * y.w - y.y * x.z * w.w - w.y * y.z * x.w
    val inv_y_z = x.x * y.z * w.w + y.x * w.z * x.w + w.x * x.z * y.w -
      x.x * w.z * y.w - y.x * x.z * w.w - w.x * y.z * x.w
    val inv_z_z = x.x * y.y * w.w + y.x * w.y * x.w + w.x * x.y * y.w -
      x.x * w.y * y.w - y.x * x.y * w.w - w.x * y.y * x.w
    val inv_w_z = x.x * y.y * w.z + y.x * w.y * x.z + w.x * x.y * y.z -
      x.x * w.y * y.z - y.x * x.y * w.z - w.x * y.y * x.z
    val inv_x_w = x.y * y.z * z.w + y.y * z.z * x.w + z.y * x.z * y.w -
      x.y * z.z * y.w - y.y * x.z * z.w - z.y * y.z * x.w
    val inv_y_w = x.x * y.z * z.w + y.x * z.z * x.w + z.x * x.z * y.w -
      x.x * z.z * y.w - y.x * x.z * z.w - z.x * y.z * x.w
    val inv_z_w = x.x * y.y * z.w + y.x * z.y * x.w + z.x * x.y * y.w -
      x.x * z.y * y.w - y.x * x.y * z.w - z.x * y.y * x.w
    val inv_w_w = x.x * y.y * z.z + y.x * z.y * x.z + z.x * x.y * y.z -
      x.x * z.y * y.z - y.x * x.y * z.z - z.x * y.y * x.z

    val inv = Matrix44(
      Vector4(inv_x_x, -inv_x_y, inv_x_z, -inv_x_w),
      Vector4(-inv_y_x, inv_y_y, -inv_y_z, inv_y_w),
      Vector4(inv_z_x, -inv_z_y, inv_z_z, -inv_z_w),
      Vector4(-inv_w_x, inv_w_y, -inv_w_z, inv_w_w))

    inv * (1 / det);
  }
  
  override def equals(other: Any) = other match {
    case m: Matrix44 => x == m.x && y == m.y && z == m.z && w == m.w
    case _ => false
  }
  override def hashCode() = 13 + x.hashCode() + y.hashCode() * 41 + z.hashCode() * 41*41 + w.hashCode() * 41*41*41
  override def toString = "Matrix44(%s, %s, %s, %s)".format(x, y, z, w)
}

object Matrix44 {
  def apply(x: Vector4, y: Vector4, z: Vector4, w: Vector4) = new Matrix44(x, y, z, w)
  def apply(m: Matrix43) = {
    val r = m.rot
    new Matrix44(
      Vector4(r.x.x, r.x.y, r.x.z, 0),
      Vector4(r.y.x, r.y.y, r.y.z, 0),
      Vector4(r.z.x, r.z.y, r.z.z, 0),
      Vector4(m.pos.x, m.pos.y, m.pos.z, 1))
  }
  val unit = new Matrix44(Vector4.xAxis, Vector4.yAxis, Vector4.zAxis, Vector4.wAxis)
  def createPerspective(width: Double, height: Double, near: Double, far: Double) =
    new Matrix44(
      Vector4(2 * near / width, 0, 0, 0),
      Vector4(0, 2 * near / height, 0, 0),
      Vector4(0, 0, -(far + near) / (far - near), -1),
      Vector4(0, 0, -2 * far * near / (far - near), 0))
}
