package refined3.numeric

import refined3.Refined

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

  describe("Divisible") {
    it("should work with literals") {
      val num: Int Refined Divisible[2] = 10
      assert(num.value == 10)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, Divisible[2]](5).isLeft)
      assert(Refined.refineV[Int, Divisible[2]](6).isRight)
      assert(Refined.refineV[Long, Divisible[2]](-5L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined Divisible[2] = 3")
    }
  }

  describe("Even") {
    it("should work with literals") {
      val num: Int Refined Even = 10
      assert(num.value == 10)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, Even](5).isLeft)
      assert(Refined.refineV[Int, Even](6).isRight)
      assert(Refined.refineV[Long, Even](-5L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined Even = 3")
    }
  }

  describe("Odd") {
    it("should work with literals") {
      val num: Int Refined Odd = 11
      assert(num.value == 11)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, Odd](5).isRight)
      assert(Refined.refineV[Int, Odd](6).isLeft)
      assert(Refined.refineV[Long, Odd](-5L).isRight)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined Odd = 2")
    }
  }

  describe("Complex Cases") {
    import refined3.generic.{And, Equal, Or}

    type MemoryPredicate = Equal[512] Or (Divisible[1024] And LessEqual[30720])
    type Memory          = Int Refined MemoryPredicate

    it("should infer type properly") {
      val memory0: Memory = 512
      val memory1: Memory = 2048
      val memory2: Memory = 4096
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, MemoryPredicate](512).isRight)
      assert(Refined.refineV[Int, MemoryPredicate](2048).isRight)
      assert(Refined.refineV[Int, MemoryPredicate](2049).isLeft)
      assert(Refined.refineV[Int, MemoryPredicate](5000).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Memory = 5000")
    }
  }
}