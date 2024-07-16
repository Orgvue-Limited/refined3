package refined3.generic

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class OrTests extends AnyFunSpec with Matchers {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal
  case object Bird extends Animal

  it("should work with Int Literals") {
    val num0: Int Refined (Equal[5] Or Equal[7]) = 5
    val num1: Int Refined (Equal[5] Or Equal[7]) = 7
    assert(num0.value == 5 && num1.value == 7)
  }

  it("should work with Long Literals") {
    val num0: Long Refined (Equal[5L] Or Equal[7L]) = 5L
    val num1: Long Refined (Equal[5L] Or Equal[7L]) = 7L
    assert(num0.value == 5 && num1.value == 7)
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val num: Int Refined (Equal[5] Or Equal[7]) = 6")
    assertDoesNotCompile("val num: Long Refined (Equal[5L] Or Equal[7L]) = 6L")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[Int, Equal[5L] Or Equal[7L]](5).isRight)
    assert(Refined.refineV[Int, Equal[5L] Or Equal[7L]](6).isLeft)
    assert(Refined.refineV[Int, Equal[5L] Or Equal[7L]](7).isRight)

    assert(Refined.refineV[Long, Equal[5L] Or Equal[7L]](5L).isRight)
    assert(Refined.refineV[Long, Equal[5L] Or Equal[7L]](6L).isLeft)
    assert(Refined.refineV[Long, Equal[5L] Or Equal[7L]](7L).isRight)
  }

  it("should validate at runtime non-primitive types") {
    assert(Refined.refineV[Cat.type, Equal[Cat.type] Or Equal[Bird.type]](Cat).isRight)
    assert(Refined.refineV[Bird.type, Equal[Cat.type] Or Equal[Bird.type]](Bird).isRight)
    assert(Refined.refineV[Dog.type, Equal[Cat.type] Or Equal[Bird.type]](Dog).isLeft)
  }
}