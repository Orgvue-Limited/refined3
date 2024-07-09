package refined

import Refined.*
import generic.*
import Proof.*

object Main extends App {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal

  //enum Animal:
  //  case Dog, Cat

  //implicit val equal: Proof[Int, Equal[5]] = Equal.validate[Int, 5](5)

  //val equal: String = Equal.validate[5, 3]

  val refinedInt: Int Refined Equal[5] = 5

  val refinedNotEqualInt: Int Refined Not[Equal[5]] = 6

  //println(refinedNotEqualInt)
  
  //inline def proof = Not.notProof[5, Equal[5]](using Equal.equalProof[5, 5])
  //inline val show = Not.notShow[5, Equal[5]](using Equal.equalShow[5, 5])

  //val refinedNotEqualInt: Int Refined Not[Equal[5]] = Refined.refineMV[5, Not[Equal[5]]](5)(using proof, show)

  //val refinedAnimal: Animal Refined Equal[Animal.Cat.type] = Animal.Cat

  println(Refined.refineV[Cat.type, Equal[Cat.type]](Cat))
  println(Refined.refineV[Cat.type, Equal[Dog.type]](Cat))

  //val refinedFailure: Int Refined Equal[5] = 4

  println(Refined.refineV[Int, Equal[5]](5))
  println(Refined.refineV[Int, Equal[5]](6))

  println(refinedInt.value)
}