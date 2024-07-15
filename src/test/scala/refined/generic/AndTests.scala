package refined.generic

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AndTests extends AnyFunSpec with Matchers {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal
  case object Bird extends Animal

  it("should work with Int Literals") {
    val num: Int Refined (Not[Equal[5]] And Not[Equal[7]]) = 6
    assert(num.value == 6)
  }

  it("should work with Long Literals") {
    val num: Long Refined (Not[Equal[5L]] And Not[Equal[7L]]) = 6L
    assert(num.value == 6L)
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val num: Int Refined (Not[Equal[5]] And Not[Equal[7]]) = 5")
    assertDoesNotCompile("val num: Int Refined (Not[Equal[5]] And Not[Equal[7]]) = 7")
    assertDoesNotCompile("val num: Long Refined (Not[Equal[5L]] And Not[Equal[7L]]) = 5L")
    assertDoesNotCompile("val num: Long Refined (Not[Equal[5L]] And Not[Equal[7L]]) = 7L")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[Int, Not[Equal[5]] And Not[Equal[7]]](5).isLeft)
    assert(Refined.refineV[Int, Not[Equal[5]] And Not[Equal[7]]](6).isRight)
    assert(Refined.refineV[Int, Not[Equal[5]] And Not[Equal[7]]](7).isLeft)

    assert(Refined.refineV[Long, Not[Equal[5L]] And Not[Equal[7L]]](5L).isLeft)
    assert(Refined.refineV[Long, Not[Equal[5L]] And Not[Equal[7L]]](6L).isRight)
    assert(Refined.refineV[Long, Not[Equal[5L]] And Not[Equal[7L]]](7L).isLeft)
  }

  it("should validate at runtime non-primitive types") {
    assert(Refined.refineV[Cat.type, Not[Equal[Cat.type]] And Not[Equal[Bird.type]]](Cat).isLeft)
    assert(Refined.refineV[Bird.type, Not[Equal[Cat.type]] And Not[Equal[Bird.type]]](Bird).isLeft)
    assert(Refined.refineV[Dog.type, Not[Equal[Cat.type]] And Not[Equal[Bird.type]]](Dog).isRight)
  }
}