package de.exoticorn.math

import org.scalatest.FunSuite

class SphereSuite extends FunSuite {
  import VectorMathSuite._

  test("merge point simple") {
    val sphere = Sphere(Vector3(2, 0, 0), 1) | Vector3(5, 0, 0)
    assert_eqv(sphere.center, Vector3(3, 0, 0))
    assert_eqf(sphere.radius, 2)
  }
  
  test("merge point includes point") {
    val sphere = Sphere(Vector3(5, 2, 7), 2)
    val point = Vector3(8, -2, 3)
    val newSphere = sphere | point
    assert_eqf((newSphere.center - point).length, newSphere.radius)
  }
}
