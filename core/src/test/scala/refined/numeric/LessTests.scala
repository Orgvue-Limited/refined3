package refined.numeric

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LessTests extends AnyFunSpec with Matchers {
  it("should work with Int Literals") {
    val num: Int Refined Less[5] = 4
    assert(num.value == 4)
  }

  it("should work with Long Literals") {
    val num: Long Refined Less[5L] = 0L
    assert(num.value == 0)
  }

  it("should work with Decimal Literals") {
    val num: Double Refined Less[5.5] = 3D
    assert(num.value == 3D)
  }

  it("should work with combinations of different literal types") {
    val refinedLessInt: Int Refined Less[5] = 4
    val refinedLessLong: Long Refined Less[5] = 3L
    val refinedLessFloat: Float Refined Less[5] = 3F
    val refinedLessDouble: Double Refined Less[5] = 3D

    val refinedLessInt2: Int Refined Less[5L] = 4
    val refinedLessLong2: Long Refined Less[5L] = 3L
    val refinedLessFloat2: Float Refined Less[5L] = 3F
    val refinedLessDouble2: Double Refined Less[5L] = 3D

    val refinedLessInt3: Int Refined Less[5.5] = 4
    val refinedLessLong3: Long Refined Less[5.5] = 3L
    val refinedLessFloat3: Float Refined Less[5.5] = 3F
    val refinedLessDouble3: Double Refined Less[5.5] = 3D
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val num: Int Refined Less[5] = 6")
    assertDoesNotCompile("val num: Long Refined Less[5L] = 6L")
    assertDoesNotCompile("val num: Int Refined Less[5] = 6L")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[Int, Less[5]](4).isRight)
    assert(Refined.refineV[Int, Less[5]](6).isLeft)
    assert(Refined.refineV[Long, Less[5L]](4L).isRight)
    assert(Refined.refineV[Long, Less[5L]](6L).isLeft)
  }
}