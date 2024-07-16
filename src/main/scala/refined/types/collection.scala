package refined.types

import refined.Refined
import refined.collection.{MinSize, MaxSize, NonEmpty, Size}
import refined.generic.And

object collection extends collection

trait collection:
  type NonEmptyArray[A]           = Array[A] Refined NonEmpty
  type NonEmptyList[A]            = List[A] Refined NonEmpty
  type NonEmptyVector[A]          = Vector[A] Refined NonEmpty

  type FixedArray[A, N]           = Array[A] Refined Size[N]
  type FixedList[A, N]            = List[A] Refined Size[N]
  type FixedVector[A, N]          = Vector[A] Refined Size[N]

  type FiniteArray[A, N]          = Array[A] Refined (MinSize[0] And MaxSize[N])
  type FiniteList[A, N]           = List[A] Refined (MinSize[0] And MaxSize[N])
  type FiniteVector[A, N]         = Vector[A] Refined (MinSize[0] And MaxSize[N])

  type NonEmptyFiniteArray[A, N]  = Array[A] Refined (MinSize[1] And MaxSize[N])
  type NonEmptyFiniteList[A, N]   = List[A] Refined (MinSize[1] And MaxSize[N])
  type NonEmptyFiniteVector[A, N] = Vector[A] Refined (MinSize[1] And MaxSize[N])