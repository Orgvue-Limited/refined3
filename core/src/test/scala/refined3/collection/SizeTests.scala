package refined3.collection

import refined3.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SizeTests extends AnyFunSpec with Matchers {
  it("should infer Refined for array constants") {
    val array: Array[Int] Refined Size[1] = Array(1)
    val array2: Array[Int] Refined Size[0] = Array[Int]()
    val array3: Array[Int] Refined Size[0] = Array.empty[Int]
    val array4: Array[Int] Refined Size[1] = scala.Array(1)
    val array5: Array[Int] Refined Size[0] = scala.Array[Int]()
    val array6: Array[Int] Refined Size[0] = scala.Array.empty[Int]
  }

  it("should infer Refined for list constants") {
    import scala.collection
    import scala.collection.immutable
    
    val list: List[Int] Refined Size[1] = scala.collection.immutable.List(1)  
    val list2: List[Int] Refined Size[1] = collection.immutable.List(1)
    val list3: List[Int] Refined Size[1] = immutable.List(1)
    val list4: List[Int] Refined Size[1] = List(1)
    val list5: List[Int] Refined Size[0] = List()
    val list6: List[Int] Refined Size[0] = List.empty[Int]
    val list7: List[Int] Refined Size[0] = List.empty
    val list8: List[Int] Refined Size[0] = immutable.List.empty
    val list9: List[Int] Refined Size[0] = collection.immutable.List.empty
    val list10: List[Int] Refined Size[0] = scala.collection.immutable.List.empty
    val list11: List[Int] Refined Size[0] = immutable.Nil
    val list12: List[Int] Refined Size[0] = collection.immutable.Nil
  }

  it("should infer Refined for vector constants") {
    import scala.collection
    import scala.collection.immutable
    
    val vector: Vector[Int] Refined Size[1] = scala.collection.immutable.Vector(1)  
    val vector2: Vector[Int] Refined Size[1] = collection.immutable.Vector(1)
    val vector3: Vector[Int] Refined Size[1] = immutable.Vector(1)
    val vector4: Vector[Int] Refined Size[1] = Vector(1)
    val vector5: Vector[Int] Refined Size[0] = Vector()
    val vector6: Vector[Int] Refined Size[0] = Vector.empty[Int]
    val vector7: Vector[Int] Refined Size[0] = Vector.empty
    val vector8: Vector[Int] Refined Size[0] = immutable.Vector.empty
    val vector9: Vector[Int] Refined Size[0] = collection.immutable.Vector.empty
    val vector10: Vector[Int] Refined Size[0] = scala.collection.immutable.Vector.empty
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("val array: Array[Int] Refined Size[1] = Array(1, 2)")
    assertDoesNotCompile("val list: List[Int] Refined Size[1] = List(1, 2)")
    assertDoesNotCompile("val vector: Vector[Int] Refined Size[1] = Vector(1, 2)")
  }
}