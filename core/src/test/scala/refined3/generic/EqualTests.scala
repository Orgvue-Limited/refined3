package refined3.generic

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EqualTests extends AnyFunSpec with Matchers {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal

  it("should work with Int Literals") {
    val num: Int Refined Equal[5] = 5
    assert(num.value == 5)
  }

  it("should work with Long Literals") {
    val num: Long Refined Equal[5L] = 5L
    assert(num.value == 5)
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val num: Int Refined Equal[5] = 6")
    assertDoesNotCompile("val num: Long Refined Equal[5L] = 6L")
    assertDoesNotCompile("val num: Int Refined Equal[5] = 6L")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[Int, Equal[5]](5).isRight)
    assert(Refined.refineV[Int, Equal[5]](6).isLeft)
    assert(Refined.refineV[Long, Equal[5L]](5L).isRight)
    assert(Refined.refineV[Long, Equal[5L]](6L).isLeft)
  }

  it("should validate at runtime non-primitive types") {
    assert(Refined.refineV[Cat.type, Equal[Cat.type]](Cat).isRight)
    assert(Refined.refineV[Cat.type, Equal[Dog.type]](Cat).isLeft)
  }
}