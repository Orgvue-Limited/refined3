package refined.collection

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MinSizeTests extends AnyFunSpec with Matchers {
  it("should infer Refined for array constants") {
    val array: Array[Int] Refined MinSize[1] = Array(1)
    val array0: Array[Int] Refined MinSize[1] = Array(1, 2)
  }

  it("should infer Refined for list constants") {
    val list: List[Int] Refined MinSize[1] = List(1)  
    val list2: List[Int] Refined MinSize[1] = List(1, 2)
  }

  it("should infer Refined for vector constants") {
    val vector: Vector[Int] Refined MinSize[1] = Vector(1)  
    val vector2: Vector[Int] Refined MinSize[1] = Vector(1, 2)
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val array: Array[Int] Refined MinSize[2] = Array(1)")
    assertDoesNotCompile("val list: List[Int] Refined MinSize[2] = List(1)")
    assertDoesNotCompile("val vector: Vector[Int] Refined MinSize[2] = Vector(1)")
  }
}