package refined

import Refined.*
import generic.*
import Proof.*

object Main extends App {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal

  val refinedInt: Int Refined Equal[5] = 5

  val refinedNotEqualInt: Int Refined Not[Equal[5]] = 6

  println(Refined.refineV[Cat.type, Equal[Cat.type]](Cat))
  println(Refined.refineV[Cat.type, Equal[Dog.type]](Cat))

  println(Refined.refineV[Int, Equal[5]](5))
  println(Refined.refineV[Int, Equal[5]](6))

  println(Refined.refineV[Int, Not[Equal[5]]](6))
  println(Refined.refineV[Int, Not[Equal[5]]](5))

  println(refinedInt.value)
}