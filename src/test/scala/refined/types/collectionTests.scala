package refined.types

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class collectionTests extends AnyFunSpec with Matchers with collection {
  describe("NonEmptyList") {
    it("should typecheck for a valid case") {
      val list: NonEmptyList[Int] = List(1, 2, 3)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val list: NonEmptyList[Int] = List()""")
      assertDoesNotCompile("""val list: NonEmptyList[Int] = Nil""")
    }
  }

  describe("FixedArray") {
    it("should typecheck for a valid case") {
      val array: FixedArray[Int, 5] = Array(1, 2, 3, 4, 5)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val array: FixedArray[Int, 5] = Array[Int]()""")
      assertDoesNotCompile("""val array: FixedArray[Int, 5] = Array(1, 2, 3)""")
    }
  }

  describe("FiniteVector") {
    it("should typecheck for a valid case") {
      val array: FiniteVector[Int, 5] = Vector(1, 2, 3)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val vector: FiniteVector[Int, 2] = Vector(1, 2, 3)""")
    }
  }

  describe("NonEmptyFiniteList") {
    it("should typecheck for a valid case") {
      val array: NonEmptyFiniteList[Int, 5] = List(1, 2, 3)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val list: NonEmptyFiniteList[Int, 2] = List()""")
      assertDoesNotCompile("""val list: NonEmptyFiniteList[Int, 2] = List(1, 2, 3)""")
      assertDoesNotCompile("""val list: NonEmptyFiniteList[Int, 2] = Nil""")
    }
  }
}