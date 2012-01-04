package de.exoticorn.math

import org.scalatest.FunSuite

class VectorMathSuite extends FunSuite {
  import VectorMathSuite._

  test("Vector2") {
    assert_eqv2(Vector2(1, 2) + Vector2(3, 4), Vector2(4, 6))
    assert_eqv2(Vector2(1, 2) - Vector2(6, 3), Vector2(-5, -1))
    assert_eqv2(Vector2(1, 2) * 2, Vector2(2, 4))
    assert_eqf(Vector2(1, 2) * Vector2(3, 4), 11)
    assert_eqf(Vector2(3, 4).length, 5)
    assert_eqf(Vector2(8, 3).normalize.length, 1)
    assert_eqf(Vector2(1, 0).angle, 0)
    assert_eqf(Vector2(0, 1).angle, math.Pi / 2)
    assert_eqf(Vector2(1, -1).angle, math.Pi / -4)
    assert_eqv2(Vector2.fromAngle(0, 2), Vector2(2, 0))
    assert_eqv2(Vector2.fromAngle(math.Pi / 2, 1), Vector2(0, 1))
  }
  
  test("Vector3") {
    assert(Vector3(1, 2, 3) + Vector3(4, 5, 6) === Vector3(5, 7, 9))
    assert(Vector3(1, 2, 3) - Vector3(4, 5, 6) === Vector3(-3, -3, -3))
    assert(Vector3(1, 2, 3) * 4 === Vector3(4, 8, 12))
    assert(Vector3(1, 2, 3) * Vector3(4, 5, 6) === 4+2*5+3*6)
    assert_eqf(Vector3(2, 3, 4).length, 5.385)
    assert_eqf(Vector3(3, -2, 7).normalize.length, 1)
  }

  test("Vector3 cross product") {
    val vec1 = Vector3(3, -2, 7)
    val vec2 = Vector3(-4, 6, -5)
    val cross = vec1 crossProduct vec2
    assert_eqf(vec1 * cross, 0)
    assert_eqf(vec2 * cross, 0)
    val scale = vec1.length * vec2.length
    val sin = vec1 * vec2 / scale
    val cos = cross.length / scale
    assert_eqf(cos * cos + sin * sin, 1)
    assert_eqv(Vector3.xAxis crossProduct Vector3.yAxis, Vector3.zAxis)
  }
  
  test("Matrix22 unit") {
    assert_unit22(Matrix22.unit)
  }
  
  test("Matrix22 transpose") {
    val m = Matrix22(Vector2(1, 2), Vector2(3, 4)).transpose
    assert_eqv2(m.x, Vector2(1, 3))
    assert_eqv2(m.y, Vector2(2, 4))
  }
  
  test("Matrix22 mulvec") {
    val m = Matrix22(Vector2(1, 2), Vector2(3, 4))
    assert_eqv2(m * Vector2(5, 6), Vector2(23, 34))
  }
  
  test("Matrix22 mulmat") {
    val m1 = Matrix22(Vector2(1, 2), Vector2(3, 4))
    val m2 = Matrix22(Vector2(5, 6), Vector2(7, 8))
    val r = m1 * m2
    assert_eqv2(r.x, Vector2(23, 34))
    assert_eqv2(r.y, Vector2(31, 46))
  }
  
  test("Matrix22 scale") {
    val m = Matrix22(Vector2(1, 2), Vector2(3, 4)) * 2
    assert_eqv2(m.x, Vector2(2, 4))
    assert_eqv2(m.y, Vector2(6, 8))
  }
  
  test("Matrix22 inverse") {
    val m = Matrix22(Vector2(1, 2), Vector2(-3, 4))
    assert_unit22(m * m.inverse)
  }

  test("Matrix32 unit") {
    assert_unit32(Matrix32.unit)
  }
  
  test("Matrix32 mulvec") {
    val m = Matrix32(Vector2(1, 2), Vector2(3, 4), Vector2(7, 8))
    assert_eqv2(m * Vector2(5, 6), Vector2(30, 42))
  }

  test("Matrix32 mulmat") {
    val m1 = Matrix32(Vector2(1, 2), Vector2(3, 4), Vector2(9, 10))
    val m2 = Matrix32(Vector2(5, 6), Vector2(7, 8), Vector2(11, 12))
    val r = m1 * m2
    assert_eqv2(r.rot.x, Vector2(23, 34))
    assert_eqv2(r.rot.y, Vector2(31, 46))
    assert_eqv2(r.pos, Vector2(11+12*3+9, 11*2+12*4+10))
  }

  test("Matrix32 inverse") {
    val m = Matrix32(Vector2(1, 2), Vector2(-3, 4), Vector2(5, 6))
    assert_unit32(m * m.inverse)
  }

  test("Matrix33 unit") {
    assert_unit33(Matrix33.unit)
  }

  test("Matrix33 transpose") {
    val m = Matrix33(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9)).transpose
    assert_eqv(m.x, Vector3(1, 4, 7))
    assert_eqv(m.y, Vector3(2, 5, 8))
    assert_eqv(m.z, Vector3(3, 6, 9))
  }

  test("Matrix33 mulmat") {
    val m1 = Matrix33(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9))
    val m2 = Matrix33(Vector3(10, 11, 12), Vector3(13, 14, 15), Vector3(16, 17, 18))
    val r = m1 * m2
    assert_eqv(r.x, Vector3(138, 171, 204))
    assert_eqv(r.y, Vector3(174, 216, 258))
    assert_eqv(r.z, Vector3(210, 261, 312))
  }

  test("Matrix33 mulvec") {
    val m = Matrix33(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9))
    assert_eqv(m * Vector3(10, 11, 12), Vector3(138, 171, 204))
  }

  test("Matrix33 scale") {
    val m = Matrix33(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9)) * 2
    assert_eqv(m.x, Vector3(2, 4, 6))
    assert_eqv(m.y, Vector3(8, 10, 12))
    assert_eqv(m.z, Vector3(14, 16, 18))
  }

  test("Matrix33 inverse") {
    val m = Matrix33(Vector3(1, 2, 3), Vector3(-4, 5, 6), Vector3(7, 8, -9))
    assert_unit33(m * m.inverse)
  }

  test("Matrix33 rotx") {
    val angle = 1.0
    val c = math.cos(angle)
    val rot = Matrix33.rotX(angle)
    assert_eqv(rot.x, Vector3.xAxis)
    assert_eqf(rot.y.y, c)
    assert_eqf(rot.z.z, c)
    assert_unit33(rot * Matrix33.rotX(-angle))
  }

  test("Matrix33 roty") {
    val angle = 1
    val c = math.cos(angle)
    val rot = Matrix33.rotY(angle)
    assert_eqv(rot.y, Vector3.yAxis)
    assert_eqf(rot.x.x, c)
    assert_eqf(rot.z.z, c)
    assert_unit33(rot * Matrix33.rotY(-angle))
  }

  test("Matrix33 rotz") {
    val angle = 1
    val c = math.cos(angle)
    val rot = Matrix33.rotZ(angle)
    assert_eqv(rot.z, Vector3.zAxis)
    assert_eqf(rot.x.x, c)
    assert_eqf(rot.y.y, c)
    assert_unit33(rot * Matrix33.rotZ(-angle))
  }

  test("Matrix43 unit") {
    assert_unit43(Matrix43.unit)
  }

  test("Matrix43 mulmat") {
    val m1 = Matrix43(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9), Vector3(10, 11, 12))
    val m2 = Matrix43(Vector3(13, 14, 15), Vector3(16, 17, 18), Vector3(19, 20, 21), Vector3(22, 23, 24))
    val r = m1 * m2
    assert_eqv(r.rot.x, Vector3(174, 216, 258))
    assert_eqv(r.rot.y, Vector3(210, 261, 312))
    assert_eqv(r.rot.z, Vector3(246, 306, 366))
    assert_eqv(r.pos, Vector3(292, 362, 432))
  }

  test("Matrix43 mulvec") {
    val m1 = Matrix43(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9), Vector3(10, 11, 12))
    assert_eqv(m1 * Vector3(13, 14, 15), Vector3(184, 227, 270))
  }

  test("Matrix43 inverse") {
    val m = Matrix43(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9), Vector3(10, 11, 12))
    assert_unit43(m * m.inverse)
  }

  test("Vector4") {
    assert_eqf(Vector4(2, 3, 4, 5).length, 7.348)
    assert_eqv4(Vector4(3, -2, 7, 4) + Vector4(-4, 6 ,-5, -2), Vector4(-1, 4, 2, 2))
    assert_eqv4(Vector4(3, -2, 7, 4) - Vector4(-4, 6, -5, -2), Vector4(7, -8, 12, 6))
    assert_eqf(Vector4(3, -2, 7, 4) * Vector4(-4, 6, -5, -2), -67)
    assert_eqv4(Vector4(3, -2, 7, 4) * 2, Vector4(6, -4, 14, 8))
    assert_eqf(Vector4(3, -2, 7, 4).normalize.length, 1)
  }

  test("Matrix44 unit") {
    assert_unit44(Matrix44.unit)
  }

  test("Matrix44 mulvec") {
    val m = Matrix44(Vector4(1, 2, 3, 4), Vector4(5, 6, 7, 8), Vector4(9, 10, 11, 12), Vector4(13, 14, 15, 16))
    assert_eqv4(m * Vector4(17, 18, 19, 20), Vector4(538, 612, 686, 760))
  }

  test("Matrix44 mulmat") {
    val m1 = Matrix44(Vector4(1, 2, 3, 4), Vector4(5, 6, 7, 8), Vector4(9, 10, 11, 12), Vector4(13, 14, 15, 16))
    val m2 = Matrix44(Vector4(17, 18, 19, 20), Vector4(21, 22, 23, 24), Vector4(25, 26, 27, 28), Vector4(29, 30, 31, 32))
    val r = m1 * m2
    assert_eqv4(r.x, Vector4(538, 612, 686, 760))
    assert_eqv4(r.y, Vector4(650, 740, 830, 920))
    assert_eqv4(r.z, Vector4(762, 868, 974, 1080))
    assert_eqv4(r.w, Vector4(874, 996, 1118, 1240))
  }

  test("Matrix44 from34") {
    val m = Matrix44(Matrix43(Vector3(1, 2, 3), Vector3(4, 5, 6), Vector3(7, 8, 9), Vector3(10, 11, 12)))
    assert_eqv4(m.x, Vector4(1, 2, 3, 0))
    assert_eqv4(m.y, Vector4(4, 5, 6, 0))
    assert_eqv4(m.z, Vector4(7, 8, 9, 0))
    assert_eqv4(m.w, Vector4(10, 11, 12, 1))
  }

  test("Matrix44 persp") {
    val m = Matrix44.createPerspective(0.8, 0.45, 0.5, 8)
    assert_eqv4(m.x, Vector4(1.25, 0, 0, 0))
    assert_eqv4(m.y, Vector4(0, 2.2222, 0, 0))
    assert_eqv4(m.z, Vector4(0, 0, -1.1333, -1))
    assert_eqv4(m.w, Vector4(0, 0, -1.0666, 0))
  }

  test("Matrix44 transpose") {
    val m = Matrix44(Vector4(1, 2, 3, 4), Vector4(5, 6, 7, 8), Vector4(9, 10, 11, 12), Vector4(13, 14, 15, 16)).transpose
    assert_eqv4(m.x, Vector4(1, 5, 9, 13))
    assert_eqv4(m.y, Vector4(2, 6, 10, 14))
    assert_eqv4(m.z, Vector4(3, 7, 11, 15))
    assert_eqv4(m.w, Vector4(4, 8, 12, 16))
  }

  test("Matrix44 mulmat43") {
    val m1 = Matrix44(Vector4(1, 2, 3, 4), Vector4(5, 6, 7, 8), Vector4(9, 10, 11, 12), Vector4(13, 14, 15, 16))
    val m2 = Matrix43(Matrix33(Vector3(17, 18, 19), Vector3(20, 21, 22), Vector3(23, 24, 25)), Vector3(26, 27, 28))
    val r = m1 * m2
    assert_eqv4(r.x, Vector4(278, 332, 386, 440))
    assert_eqv4(r.y, Vector4(323, 386, 449, 512))
    assert_eqv4(r.z, Vector4(368, 440, 512, 584))
    assert_eqv4(r.w, Vector4(426, 508, 590, 672))
  }

  test("Matrix44 inverse") {
    val m = Matrix44(Vector4(1, 2, 3, 4), Vector4(5, 6, 7, 8), Vector4(9, 10, 11, 12), Vector4(13, 14, 15, 16))
    assert_unit44(m * m.inverse)
  }
}

object VectorMathSuite {
  import java.lang.AssertionError

  def assert_eqf(a: Double, b: Double, epsilon: Double = 0.001) {
    if(math.abs(a - b) > epsilon) {
      throw new AssertionError("%f != %f".format(a, b))
    }
  }

  def assert_eqv2(a: Vector2, b: Vector2, epsilon: Double = 0.001) {
    if(math.abs(a.x - b.x) > epsilon ||
       math.abs(a.y - b.y) > epsilon) {
	 throw new AssertionError("%s != %s".format(a, b))
       }
  }

  def assert_eqv(a: Vector3, b: Vector3, epsilon: Double = 0.001) {
    if(math.abs(a.x - b.x) > epsilon ||
       math.abs(a.y - b.y) > epsilon ||
       math.abs(a.z - b.z) > epsilon) {
	 throw new AssertionError("%s != %s".format(a, b))
       }
  }

  def assert_eqv4(a: Vector4, b: Vector4, epsilon: Double = 0.001) {
    if(math.abs(a.x - b.x) > epsilon ||
       math.abs(a.y - b.y) > epsilon ||
       math.abs(a.z - b.z) > epsilon ||
       math.abs(a.w - b.w) > epsilon) {
	 throw new AssertionError("%s != %s".format(a, b))
       }
  }
  
  def assert_unit22(m: Matrix22, epsilon: Double = 0.001) {
    assert_eqv2(m.x, Vector2.xAxis, epsilon)
    assert_eqv2(m.y, Vector2.yAxis, epsilon)
  }

  def assert_unit32(m: Matrix32, epsilon: Double = 0.001) {
    assert_unit22(m.rot, epsilon)
    assert_eqv2(m.pos, Vector2(0, 0), epsilon)
  }

  def assert_unit33(m: Matrix33, epsilon: Double = 0.001) {
    assert_eqv(m.x, Vector3.xAxis, epsilon)
    assert_eqv(m.y, Vector3.yAxis, epsilon)
    assert_eqv(m.z, Vector3.zAxis, epsilon)
  }

  def assert_unit43(m: Matrix43, epsilon: Double = 0.001) {
    assert_unit33(m.rot, epsilon)
    assert_eqv(m.pos, Vector3(0, 0, 0), epsilon)
  }

  def assert_unit44(m: Matrix44, epsilon: Double = 0.001) {
    assert_eqv4(m.x, Vector4.xAxis)
    assert_eqv4(m.y, Vector4.yAxis)
    assert_eqv4(m.z, Vector4.zAxis)
    assert_eqv4(m.w, Vector4.wAxis)
  }
}
