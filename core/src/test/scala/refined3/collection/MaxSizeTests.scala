package refined3.collection

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MaxSizeTests extends AnyFunSpec with Matchers {
  it("should infer Refined for array constants") {
    val array: Array[Int] Refined MaxSize[1] = Array(1)
    val array0: Array[Int] Refined MaxSize[1] = Array[Int]()
    val array1: Array[Int] Refined MaxSize[2] = Array(1, 2)
  }

  it("should infer Refined for list constants") {
    val list: List[Int] Refined MaxSize[1]  = List(1)  
    val list2: List[Int] Refined MaxSize[2] = List(1, 2)
    val list3: List[Int] Refined MaxSize[2] = Nil
  }

  it("should infer Refined for vector constants") {
    val vector: Vector[Int] Refined MaxSize[2]  = Vector(1)  
    val vector2: Vector[Int] Refined MaxSize[2] = Vector(1, 2)
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val array: Array[Int] Refined MaxSize[1] = Array(1, 2)")
    assertDoesNotCompile("val list: List[Int] Refined MaxSize[1] = List(1, 2)")
    assertDoesNotCompile("val vector: Vector[Int] Refined MaxSize[1] = Vector(1, 2)")
  }
}