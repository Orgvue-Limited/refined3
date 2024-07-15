package refined

import Refined.*
import generic.*
import numeric.*
import Proof.*
import refined.internal.WitnessAs

object Main extends App {
  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal

  val refinedInt: Int Refined Equal[5] = 5

  val refinedNotEqualInt: Int Refined Not[Equal[5]] = 6

  val refinedAndInt: Int Refined (Not[Equal[0]] And Not[Equal[2]]) = 1
  
  val refinedOrInt: Int Refined (Equal[0] Or Equal[2]) = 2

  val refinedLessInt: Int Refined Less[5] = 4
  val refinedLessLong: Long Refined Less[5] = 3L
  val refinedLessFloat: Float Refined Less[5] = 3F
  val refinedLessDouble: Double Refined Less[5] = 3D

  val refinedLessInt2: Int Refined Less[5L] = 4
  val refinedLessLong2: Long Refined Less[5L] = 3L
  val refinedLessFloat2: Float Refined Less[5L] = 3F
  val refinedLessDouble2: Double Refined Less[5L] = 3D

  val refinedLessInt3: Int Refined Less[5.5] = 4
  val refinedLessLong3: Long Refined Less[5.5] = 3L
  val refinedLessFloat3: Float Refined Less[5.5] = 3F
  val refinedLessDouble3: Double Refined Less[5.5] = 3D

  //val proof = Less.lessProof[4, 5](using refined.internal.WitnessAs.singletonWitnessAs[Int, 5]) //(using Numeric.IntIsIntegral)

  //  println(proof)

  //val refinedLessInt3: Long Refined Less[5] = Refined.refineMV[4L, Less[5]](4L)
  //val refinedLessInt3: Long Refined Less[5] = Refined.refineMV[4L, Less[5]](4L)(using Less.lessProof[4L, 5](using WitnessAs.longWitnessAsInt[4L]))

  println(Refined.refineV[Cat.type, Equal[Cat.type]](Cat))
  println(Refined.refineV[Cat.type, Equal[Dog.type]](Cat))

  println(Refined.refineV[Int, Equal[5]](5))
  println(Refined.refineV[Int, Equal[5]](6))

  println(Refined.refineV[Int, Not[Equal[5]]](6))
  println(Refined.refineV[Int, Not[Equal[5]]](5))

  println(Refined.refineV[Int, (Not[Equal[0]] And Not[Equal[2]])](0))
  println(Refined.refineV[Int, (Not[Equal[0]] And Not[Equal[2]])](1))

  println(Refined.refineV[Int, (Equal[0] Or Equal[2])](0))
  println(Refined.refineV[Int, (Equal[0] Or Equal[2])](1))

  println(Less.lessValidate[Int, 5](4))
  println(Less.lessValidate[Int, 5](6))
}