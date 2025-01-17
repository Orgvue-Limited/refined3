package refined3.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class collectionTests extends AnyFunSpec with Matchers with collection {
  describe("NonEmptyList") {
    it("should typecheck for a valid case") {
      val list: NonEmptyList[Int] = List(1, 2, 3)
      val nonEmptyList = NonEmptyList[Int](List(1, 2, 3))
      val nonEmptyList2 = NonEmptyList.of(1, 2, 3)
      assert(nonEmptyList2.value == List(1, 2, 3))
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val list: NonEmptyList[Int] = List()""")
      assertDoesNotCompile("""val list: NonEmptyList[Int] = Nil""")
    }

    it("should validate at runtime correctly") {
      assert(NonEmptyList[Int].from(List(1)).isRight)
      assert(NonEmptyList[Int].from(List()).isLeft)
      assert(NonEmptyList[Int].from(Nil).isLeft)
    }
  }

  describe("NonEmptyVector") {
    it("should typecheck for a valid case") {
      val vector: NonEmptyVector[Int] = Vector(1, 2, 3)
      val nonEmptyVector = NonEmptyVector[Int](Vector(1, 2, 3))
      val nonEmptyVector2 = NonEmptyVector.of(1, 2, 3)
      assert(nonEmptyVector2.value == Vector(1, 2, 3))
    }
  }

  describe("NonEmptyArray") {
    it("should typecheck for a valid case") {
      val array: NonEmptyArray[Int] = Array(1, 2, 3)
      val nonEmptyArray = NonEmptyArray[Int](Array(1, 2, 3))
      val nonEmptyArray2 = NonEmptyArray.of(1, 2, 3)
      assert(nonEmptyArray2.value.toList == List(1, 2, 3))
    }
  }

  describe("FixedArray") {
    it("should typecheck for a valid case") {
      val array: FixedArray[Int, 5] = Array(1, 2, 3, 4, 5)

      val array2 = FixedArray.of[Int, 5](1, 2, 3, 4, 5)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val array: FixedArray[Int, 5] = Array[Int]()""")
      assertDoesNotCompile("""val array: FixedArray[Int, 5] = Array(1, 2, 3)""")
    }

    it("should validate at runtime correctly") {
      assert(FixedArray[Int, 5].from(Array(1, 2, 3, 4, 5)).isRight)
      assert(FixedArray[Int, 5].from(Array(1, 2, 3, 4)).isLeft)
      assert(FixedArray[Int, 5].from(Array[Int]()).isLeft)
    }
  }

  describe("FixedList") {
    it("should typecheck for a valid case") {
      val list: FixedList[Int, 5] = List(1, 2, 3, 4, 5)
      val list2 = FixedList.of[Int, 5](1, 2, 3, 4, 5)
    }
  }

  describe("FixedVector") {
    it("should typecheck for a valid case") {
      val vector: FixedVector[Int, 5] = Vector(1, 2, 3, 4, 5)
      val vector2 = FixedVector.of[Int, 5](1, 2, 3, 4, 5)
    }
  }

  describe("FiniteVector") {
    it("should typecheck for a valid case") {
      val array: FiniteVector[Int, 5] = Vector(1, 2, 3)
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val vector: FiniteVector[Int, 2] = Vector(1, 2, 3)""")
    }

    it("should validate at runtime correctly") {
      assert(FiniteVector[Int, 5].from(Vector(1, 2, 3, 4, 5, 6)).isLeft)
      assert(FiniteVector[Int, 5].from(Vector(1, 2, 3, 4, 5)).isRight)
      assert(FiniteVector[Int, 5].from(Vector(1, 2, 3, 4)).isRight)
      assert(FiniteVector[Int, 5].from(Vector()).isRight)
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

    it("should validate at runtime correctly") {
      assert(NonEmptyFiniteList[Int, 5].from(List(1, 2, 3, 4, 5, 6)).isLeft)
      assert(NonEmptyFiniteList[Int, 5].from(List(1, 2, 3, 4, 5)).isRight)
      assert(NonEmptyFiniteList[Int, 5].from(List(1, 2, 3, 4)).isRight)
      assert(NonEmptyFiniteList[Int, 5].from(List()).isLeft)
      assert(NonEmptyFiniteList[Int, 5].from(Nil).isLeft)
    }
  }
}