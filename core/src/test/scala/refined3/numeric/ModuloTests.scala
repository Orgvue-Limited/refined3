package refined3.numeric

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ModuloTests extends AnyFunSpec with Matchers {
  it("should work with Int Literals") {
    val num: Int Refined Modulo[5, 1] = 6
    assert(num.value == 6)
  }

  it("should work with Long Literals") {
    val num: Long Refined Modulo[5L, 1L] = 6L
    assert(num.value == 6L)
  }

  it("should work with Decimal Literals") {
    val num: Double Refined Modulo[5.5, 1.0] = 6.5
    assert(num.value == 6.5)
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val num: Int Refined Modulo[5, 4] = 3")
    assertDoesNotCompile("val num: Long Refined Modulo[5L, 3L] = 4L")
    assertDoesNotCompile("val num: Int Refined Modulo[5, 2] = 3L")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[Int, Modulo[5, 1]](6).isRight)
    assert(Refined.refineV[Int, Modulo[5, 1]](5).isLeft)
  }
}