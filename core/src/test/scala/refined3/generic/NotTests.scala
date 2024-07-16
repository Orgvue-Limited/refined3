package refined3.generic

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NotTests extends AnyFunSpec with Matchers {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal

  it("should work with Int Literals") {
    val num: Int Refined Not[Equal[5]] = 6
    assert(num.value == 6)
  }

  it("should work with Long Literals") {
    val num: Long Refined Not[Equal[5L]] = 6L
    assert(num.value == 6L)
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val num: Int Refined Not[Equal[5]] = 5")
    assertDoesNotCompile("val num: Long Refined Not[Equal[5L]] = 5L")
    assertDoesNotCompile("val num: Int Refined Not[Equal[5]] = 5L")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[Int, Not[Equal[5]]](5).isLeft)
    assert(Refined.refineV[Int, Not[Equal[5]]](6).isRight)
    assert(Refined.refineV[Long, Not[Equal[5L]]](5L).isLeft)
    assert(Refined.refineV[Long, Not[Equal[5L]]](6L).isRight)
  }

  it("should validate at runtime non-primitive types") {
    assert(Refined.refineV[Cat.type, Not[Equal[Cat.type]]](Cat).isLeft)
    assert(Refined.refineV[Cat.type, Not[Equal[Dog.type]]](Cat).isRight)
  }
}