package refined3.numeric

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class IntervalTests extends AnyFunSpec with Matchers {
  import Interval.*

  describe("Open") {
    it("should work with literals") {
      val num0: Int Refined Open[1, 4] = 2
      val num1: Int Refined Open[1, 4] = 3
      assert(num0.value == 2 && num1.value == 3)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, Open[1, 3]](1).isLeft)
      assert(Refined.refineV[Int, Open[1, 3]](2).isRight)
      assert(Refined.refineV[Long, Open[1L, 3L]](3L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined Open[1, 3] = 1")
      assertDoesNotCompile("val num: Int Refined Open[1, 3] = 3")
    }
  }

  describe("OpenClosed") {
    it("should work with literals") {
      val num0: Int Refined OpenClosed[1, 3] = 2
      val num1: Int Refined OpenClosed[1, 3] = 3
      assert(num0.value == 2 && num1.value == 3)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, OpenClosed[1, 3]](1).isLeft)
      assert(Refined.refineV[Int, OpenClosed[1, 3]](2).isRight)
      assert(Refined.refineV[Long, OpenClosed[1L, 3L]](3L).isRight)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined OpenClosed[1, 3] = 1")
      assertDoesNotCompile("val num: Int Refined OpenClosed[1, 3] = 4")
    }
  }

  describe("ClosedOpen") {
    it("should work with literals") {
      val num0: Int Refined ClosedOpen[1, 3] = 1
      val num1: Int Refined ClosedOpen[1, 3] = 2
      assert(num0.value == 1 && num1.value == 2)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, ClosedOpen[1, 3]](1).isRight)
      assert(Refined.refineV[Int, ClosedOpen[1, 3]](2).isRight)
      assert(Refined.refineV[Long, ClosedOpen[1L, 3L]](3L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined ClosedOpen[1, 3] = 0")
      assertDoesNotCompile("val num: Int Refined ClosedOpen[1, 3] = 3")
    }
  }

  describe("Closed") {
    it("should work with literals") {
      val num0: Int Refined Closed[1, 3] = 1
      val num1: Int Refined Closed[1, 3] = 3
      assert(num0.value == 1 && num1.value == 3)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[Int, Closed[1, 3]](0).isLeft)
      assert(Refined.refineV[Int, Closed[1, 3]](1).isRight)
      assert(Refined.refineV[Int, Closed[1, 3]](2).isRight)
      assert(Refined.refineV[Long, Closed[1L, 3L]](3L).isRight)
      assert(Refined.refineV[Long, Closed[1L, 3L]](4L).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: Int Refined Closed[1, 3] = 0")
      assertDoesNotCompile("val num: Int Refined Closed[1, 3] = 4")
    }
  }
}