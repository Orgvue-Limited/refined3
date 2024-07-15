package refined.collection

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NumericTests extends AnyFunSpec with Matchers {
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