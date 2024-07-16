package refined.collection

import refined.Refined
import refined.generic.{Equal, Or}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class HeadTests extends AnyFunSpec with Matchers {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal
  case object Bird extends Animal

  it("should work with list constants") {
    val list: List[Int] Refined Head[Equal[1]] = List(1, 1, 1)
    assert(list.value == List(1, 1, 1))
  }

  it("should work with array constants") {
    val list: Array[Int] Refined Head[Equal[1]] = Array(1, 1, 1)
    assert(list.value.toList == List(1, 1, 1))
  }

  it("should work with vector constants") {
    val list: Vector[Int] Refined Head[Equal[1]] = Vector(1, 1, 1)
    assert(list.value == Vector(1, 1, 1))
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val list: List[Int] Refined Head[Equal[1]] = List(2)")
    assertDoesNotCompile("val list: List[Int] Refined Head[Equal[1]] = List(2, 1)")
    assertDoesNotCompile("val list: Array[Int] Refined Head[Equal[1]] = Array(2)")
    assertDoesNotCompile("val list: Array[Int] Refined Head[Equal[1]] = Array(2, 1)")
    assertDoesNotCompile("val list: Vector[Int] Refined Head[Equal[1]] = Vector(2)")
    assertDoesNotCompile("val list: Vector[Int] Refined Head[Equal[1]] = Vector(2, 1)")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[List[Long], Head[Equal[1L]]](List(1L, 2L)).isRight)
    assert(Refined.refineV[List[Long], Head[Equal[1L]]](List(2L, 1L)).isLeft)
    assert(Refined.refineV[List[Boolean], Head[Equal[true]]](List()).isLeft)

    assert(Refined.refineV[Array[Long], Head[Equal[1L]]](Array(1L, 2L)).isRight)
    assert(Refined.refineV[Array[Long], Head[Equal[1L]]](Array(2L, 1L)).isLeft)
    assert(Refined.refineV[Array[Boolean], Head[Equal[true]]](Array[Boolean]()).isLeft)

    assert(Refined.refineV[Vector[Long], Head[Equal[1L]]](Vector(1L, 2L)).isRight)
    assert(Refined.refineV[Vector[Long], Head[Equal[1L]]](Vector(2L, 1L)).isLeft)
    assert(Refined.refineV[Vector[Boolean], Head[Equal[true]]](Vector()).isLeft)
  }

  it("should validate at runtime non-primitive types") {
    assert(Refined.refineV[List[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](List(Cat, Dog)).isRight)
    assert(Refined.refineV[List[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](List(Bird, Dog)).isRight)
    assert(Refined.refineV[List[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](List(Dog, Cat)).isLeft)
    assert(Refined.refineV[List[Animal], Head[Equal[Dog.type]]](Nil).isLeft)

    assert(Refined.refineV[Array[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](Array(Cat, Dog)).isRight)
    assert(Refined.refineV[Array[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](Array(Bird, Dog)).isRight)
    assert(Refined.refineV[Array[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](Array(Dog, Cat)).isLeft)
    assert(Refined.refineV[Array[Animal], Head[Equal[Dog.type]]](Array[Animal]()).isLeft)

    assert(Refined.refineV[Vector[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](Vector(Cat, Dog)).isRight)
    assert(Refined.refineV[Vector[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](Vector(Bird, Dog)).isRight)
    assert(Refined.refineV[Vector[Animal], Head[Equal[Cat.type] Or Equal[Bird.type]]](Vector(Dog, Cat)).isLeft)
    assert(Refined.refineV[Vector[Animal], Head[Equal[Dog.type]]](Vector()).isLeft)
  }
}