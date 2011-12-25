package de.exoticorn.math

import org.scalatest.FunSuite

class PlaneSuite extends FunSuite {
  import VectorMathSuite._
  
  test("height") {
    val plane = Plane(Vector3(0, 1, 0), -2)
    assert_eqf(plane.height(Vector3(10, 5, 3)), 7)
  }

  test("from vertices") {
    val plane = Plane(Vector3(1, 5, 3), Vector3(4, 6, 3), Vector3(2, 8, 3))
    assert_eqv(plane.normal, Vector3(0, 0, -1))
    assert_eqf(plane.t, -3)
  }
}
