package refined.numeric

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NumericTests extends AnyFunSpec with Matchers {
  describe("LessEqual") {
    it("should work with literals") {
      val num0: Int Refined LessEqual[5] = 5
      val num1: Int Refined LessEqual[5] = 4
      assert(num0.value == 5 && num1.value == 4)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, LessEqual[5]](5).isRight)
      assert(Refined.refineV[Int, LessEqual[5]](5).isRight)
      assert(Refined.refineV[Long, LessEqual[5L]](6L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined LessEqual[5] = 6")
    }
  }
  
  describe("GreaterEqual") {
    it("should work with literals") {
      val num0: Int Refined GreaterEqual[5] = 5
      val num1: Int Refined GreaterEqual[5] = 6
      assert(num0.value == 5 && num1.value == 6)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, GreaterEqual[5]](5).isRight)
      assert(Refined.refineV[Int, GreaterEqual[5]](6).isRight)
      assert(Refined.refineV[Long, GreaterEqual[5L]](4L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined GreaterEqual[5] = 4")
    }
  }

  describe("Positive") {
    it("should work with literals") {
      val num: Int Refined Positive = 1
      assert(num.value == 1)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, Positive](5).isRight)
      assert(Refined.refineV[Int, Positive](0).isLeft)
      assert(Refined.refineV[Long, Positive](-5L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined Positive = -1")
    }
  }

  describe("NonPositive") {
    it("should work with literals") {
      val num: Int Refined NonPositive = 0
      assert(num.value == 0)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, NonPositive](5).isLeft)
      assert(Refined.refineV[Int, NonPositive](0).isRight)
      assert(Refined.refineV[Long, NonPositive](-5L).isRight)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined NonPositive = 1")
    }
  }

  describe("Negative") {
    it("should work with literals") {
      val num: Int Refined Negative = -1
      assert(num.value == -1)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, Negative](5).isLeft)
      assert(Refined.refineV[Int, Negative](0).isLeft)
      assert(Refined.refineV[Long, Negative](-5L).isRight)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined Negative = 1")
    }
  }

  describe("NonNegative") {
    it("should work with literals") {
      val num: Int Refined NonNegative = 0
      assert(num.value == 0)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, NonNegative](5).isRight)
      assert(Refined.refineV[Int, NonNegative](0).isRight)
      assert(Refined.refineV[Long, NonNegative](-5L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined NonNegative = -1")
    }
  }
}