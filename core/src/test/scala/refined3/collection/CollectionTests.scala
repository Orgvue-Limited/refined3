package refined3.collection

import refined3.Refined
import refined3.generic.Equal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CollectionTests extends AnyFunSpec with Matchers {
  describe("Empty") {
    it("should work with constants") {
      val num0: List[Int] Refined Empty   = Nil
      val num1: Array[Int] Refined Empty  = Array[Int]()
      val num2: Vector[Int] Refined Empty = Vector()
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[List[Int], Empty](Nil).isRight)
      assert(Refined.refineV[Array[Int], Empty](Array[Int]()).isRight)
      assert(Refined.refineV[Vector[Int], Empty](Vector()).isRight)

      assert(Refined.refineV[List[Int], Empty](List(1)).isLeft)
      assert(Refined.refineV[Array[Int], Empty](Array[Int](1)).isLeft)
      assert(Refined.refineV[Vector[Int], Empty](Vector(1)).isLeft)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val list: List[Int] Refined Empty = List(1)")
      assertDoesNotCompile("val array: Array[Int] Refined Empty = Array(1)")
      assertDoesNotCompile("val vector: Vector[Int] Refined Empty = Vector(1)")
    }
  }

  describe("NonEmpty") {
    it("should work with constants") {
      val num0: List[Int] Refined NonEmpty   = List(1)
      val num1: Array[Int] Refined NonEmpty  = Array[Int](1)
      val num2: Vector[Int] Refined NonEmpty = Vector(1)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[List[Int], NonEmpty](Nil).isLeft)
      assert(Refined.refineV[Array[Int], NonEmpty](Array[Int]()).isLeft)
      assert(Refined.refineV[Vector[Int], NonEmpty](Vector()).isLeft)

      assert(Refined.refineV[List[Int], NonEmpty](List(1)).isRight)
      assert(Refined.refineV[Array[Int], NonEmpty](Array[Int](1)).isRight)
      assert(Refined.refineV[Vector[Int], NonEmpty](Vector(1)).isRight)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val list: List[Int] Refined NonEmpty = List()")
      assertDoesNotCompile("val array: Array[Int] Refined NonEmpty = Array[Int]()")
      assertDoesNotCompile("val vector: Vector[Int] Refined NonEmpty = Vector()")
    }
  }

  describe("Exists") {
    it("should work with constants") {
      val list: List[Int] Refined Exists[Equal[1]]   = List(1, 2, 3)
      val array: Array[Int] Refined Exists[Equal[1]] = Array(1, 2, 3)
      val vector: Vector[Int] Refined Exists[Equal[1]] = Vector(1, 2, 3)
    }

    it("should validate at runtime correctly") {
      assert(Refined.refineV[List[Int], Exists[Equal[1]]](Nil).isLeft)
      assert(Refined.refineV[List[Int], Exists[Equal[1]]](List(2, 2)).isLeft)
      assert(Refined.refineV[List[Int], Exists[Equal[1]]](List(1, 2)).isRight)

      assert(Refined.refineV[Array[Int], Exists[Equal[1]]](Array()).isLeft)
      assert(Refined.refineV[Array[Int], Exists[Equal[1]]](Array(2, 2)).isLeft)
      assert(Refined.refineV[Array[Int], Exists[Equal[1]]](Array(1, 2)).isRight)

      assert(Refined.refineV[Vector[Int], Exists[Equal[1]]](Vector()).isLeft)
      assert(Refined.refineV[Vector[Int], Exists[Equal[1]]](Vector(2, 2)).isLeft)
      assert(Refined.refineV[Vector[Int], Exists[Equal[1]]](Vector(1, 2)).isRight)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val list: List[Int] Refined Exists[Equal[1]] = List()")
      assertDoesNotCompile("val list: List[Int] Refined Exists[Equal[1]] = List(2)")

      assertDoesNotCompile("val array: Array[Int] Refined Exists[Equal[1]] = Array()")
      assertDoesNotCompile("val array: Array[Int] Refined Exists[Equal[1]] = Array(2)")

      assertDoesNotCompile("val vector: Vector[Int] Refined Exists[Equal[1]] = Vector()")
      assertDoesNotCompile("val vector: Vector[Int] Refined Exists[Equal[1]] = Vector(2)")
    }
  }

  describe("Contains") {
    it("should work with constants") {
      val list: List[Int] Refined Contains[1]   = List(1, 2, 3)
      val array: Array[Int] Refined Contains[1] = Array(1, 2, 3)
      val vector: Vector[Int] Refined Contains[1] = Vector(1, 2, 3)
    }
  }
}