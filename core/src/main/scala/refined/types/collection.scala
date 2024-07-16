package refined.types

import refined.{Refined, RefinedTypeOps}
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

  object NonEmptyArray:
    inline def apply[A]: RefinedTypeOps[Array[A], NonEmpty] =
      new RefinedTypeOps[Array[A], NonEmpty]

  object NonEmptyList:
    inline def apply[A]: RefinedTypeOps[List[A], NonEmpty] =
      new RefinedTypeOps[List[A], NonEmpty]

  object NonEmptyVector:
    inline def apply[A]: RefinedTypeOps[Vector[A], NonEmpty] =
      new RefinedTypeOps[Vector[A], NonEmpty]

  object FixedArray:
    inline def apply[A, N <: Int]: RefinedTypeOps[Array[A], Size[N]] =
      new RefinedTypeOps[Array[A], Size[N]]

  object FixedList:
    inline def apply[A, N <: Int]: RefinedTypeOps[List[A], Size[N]] =
      new RefinedTypeOps[List[A], Size[N]]

  object FixedVector:
    inline def apply[A, N <: Int]: RefinedTypeOps[Vector[A], Size[N]] =
      new RefinedTypeOps[Vector[A], Size[N]]

  object FiniteArray:
    inline def apply[A, N <: Int]: RefinedTypeOps[Array[A], MinSize[0] And MaxSize[N]] =
      new RefinedTypeOps[Array[A], MinSize[0] And MaxSize[N]]

  object FiniteList:
    inline def apply[A, N <: Int]: RefinedTypeOps[List[A], MinSize[0] And MaxSize[N]] =
      new RefinedTypeOps[List[A], MinSize[0] And MaxSize[N]]

  object FiniteVector:
    inline def apply[A, N <: Int]: RefinedTypeOps[Vector[A], MinSize[0] And MaxSize[N]] =
      new RefinedTypeOps[Vector[A], MinSize[0] And MaxSize[N]]

  object NonEmptyFiniteArray:
    inline def apply[A, N <: Int]: RefinedTypeOps[Array[A], MinSize[1] And MaxSize[N]] =
      new RefinedTypeOps[Array[A], MinSize[1] And MaxSize[N]]

  object NonEmptyFiniteList:
    inline def apply[A, N <: Int]: RefinedTypeOps[List[A], MinSize[1] And MaxSize[N]] =
      new RefinedTypeOps[List[A], MinSize[1] And MaxSize[N]]

  object NonEmptyFiniteVector:
    inline def apply[A, N <: Int]: RefinedTypeOps[Vector[A], MinSize[1] And MaxSize[N]] =
      new RefinedTypeOps[Vector[A], MinSize[1] And MaxSize[N]]