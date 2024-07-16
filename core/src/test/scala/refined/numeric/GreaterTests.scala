package refined.numeric

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GreaterTests extends AnyFunSpec with Matchers {
  it("should work with Int Literals") {
    val num: Int Refined Greater[5] = 6
    assert(num.value == 6)
  }

  it("should work with Long Literals") {
    val num: Long Refined Greater[5L] = 6L
    assert(num.value == 6)
  }

  it("should work with Decimal Literals") {
    val num: Double Refined Greater[5.5] = 6D
    assert(num.value == 6D)
  }

  it("should work with combinations of different literal types") {
    val refinedGreaterInt: Int Refined Greater[5] = 6
    val refinedGreaterLong: Long Refined Greater[0] = 8L
    val refinedGreaterFloat: Float Refined Greater[5] = 10F
    val refinedGreaterDouble: Double Refined Greater[5] = 9D

    val refinedGreaterInt2: Int Refined Greater[5L] = 6
    val refinedGreaterLong2: Long Refined Greater[5L] = 8L
    val refinedGreaterFloat2: Float Refined Greater[5L] = 10F
    val refinedGreaterDouble2: Double Refined Greater[5L] = 9D

    val refinedGreaterInt3: Int Refined Greater[5.5] = 6
    val refinedGreaterLong3: Long Refined Greater[5.5] = 8L
    val refinedGreaterFloat3: Float Refined Greater[5.5] = 10F
    val refinedGreaterDouble3: Double Refined Greater[5.5] = 9D
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val num: Int Refined Greater[5] = 3")
    assertDoesNotCompile("val num: Long Refined Greater[5L] = 4L")
    assertDoesNotCompile("val num: Int Refined Greater[5] = 3L")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[Int, Greater[5]](6).isRight)
    assert(Refined.refineV[Int, Greater[5]](4).isLeft)
    assert(Refined.refineV[Long, Greater[5L]](6L).isRight)
    assert(Refined.refineV[Long, Greater[5L]](4L).isLeft)
  }
}