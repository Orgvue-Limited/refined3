package refined3.collection

import refined3.Refined
import refined3.generic.{Equal, Or}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ForallTests extends AnyFunSpec with Matchers {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal
  case object Bird extends Animal

  it("should work with list constants") {
    val list0: List[Int] Refined Forall[Equal[1]] = List(1, 1, 1)
    val list1: List[Int] Refined Forall[Equal[1]] = Nil
    assert(list0.value == List(1, 1, 1) && list1.value == Nil)
  }

  it("should work with array constants") {
    val list0: Array[Int] Refined Forall[Equal[1]] = Array(1, 1, 1)
    val list1: Array[Int] Refined Forall[Equal[1]] = Array[Int]()
    assert(list0.value.toList == List(1, 1, 1) && list1.value.toList == Nil)
  }

  it("should work with vector constants") {
    val list0: Vector[Int] Refined Forall[Equal[1]] = Vector(1, 1, 1)
    val list1: Vector[Int] Refined Forall[Equal[1]] = Vector[Int]()
    assert(list0.value == Vector(1, 1, 1) && list1.value == Vector())
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val list: List[Int] Refined Forall[Equal[1]] = List(2)")
    assertDoesNotCompile("val list: List[Int] Refined Forall[Equal[1]] = List(1, 2)")
    assertDoesNotCompile("val list: Array[Int] Refined Forall[Equal[1]] = Array(2)")
    assertDoesNotCompile("val list: Array[Int] Refined Forall[Equal[1]] = Array(1, 2)")
    assertDoesNotCompile("val list: Vector[Int] Refined Forall[Equal[1]] = Vector(2)")
    assertDoesNotCompile("val list: Vector[Int] Refined Forall[Equal[1]] = Vector(1, 2)")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[List[Long], Forall[Equal[1L]]](List(1L, 1L, 1L)).isRight)
    assert(Refined.refineV[List[Long], Forall[Equal[1L]]](List(1L, 2L, 1L)).isLeft)
    assert(Refined.refineV[List[Boolean], Forall[Equal[true]]](List()).isRight)

    assert(Refined.refineV[Array[Long], Forall[Equal[1L]]](Array(1L, 1L, 1L)).isRight)
    assert(Refined.refineV[Array[Long], Forall[Equal[1L]]](Array(1L, 2L, 1L)).isLeft)
    assert(Refined.refineV[Array[Boolean], Forall[Equal[true]]](Array[Boolean]()).isRight)

    assert(Refined.refineV[Vector[Long], Forall[Equal[1L]]](Vector(1L, 1L, 1L)).isRight)
    assert(Refined.refineV[Vector[Long], Forall[Equal[1L]]](Vector(1L, 2L, 1L)).isLeft)
    assert(Refined.refineV[Vector[Boolean], Forall[Equal[true]]](Vector[Boolean]()).isRight)
  }

  it("should validate at runtime non-primitive types") {
    assert(Refined.refineV[List[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](List(Cat, Cat)).isRight)
    assert(Refined.refineV[List[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](List(Cat, Bird)).isRight)
    assert(Refined.refineV[List[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](List(Bird, Bird)).isRight)

    assert(Refined.refineV[List[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](List(Dog, Cat)).isLeft)
    assert(Refined.refineV[List[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](List(Cat, Dog)).isLeft)
    assert(Refined.refineV[List[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](List(Dog, Dog)).isLeft)

    assert(Refined.refineV[List[Animal], Forall[Equal[Dog.type]]](Nil).isRight)

    assert(Refined.refineV[Array[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](Array(Cat, Cat)).isRight)
    assert(Refined.refineV[Array[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](Array(Cat, Bird)).isRight)
    assert(Refined.refineV[Array[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](Array(Bird, Bird)).isRight)

    assert(Refined.refineV[Vector[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](Vector(Cat, Cat)).isRight)
    assert(Refined.refineV[Vector[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](Vector(Cat, Bird)).isRight)
    assert(Refined.refineV[Vector[Animal], Forall[Equal[Cat.type] Or Equal[Bird.type]]](Vector(Bird, Bird)).isRight)
  }
}