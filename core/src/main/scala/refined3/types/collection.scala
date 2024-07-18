package refined3.types

import refined3.{Refined, RefinedTypeOps}
import refined3.collection.{MinSize, MaxSize, NonEmpty, Size}
import refined3.generic.And

import scala.reflect.ClassTag

object collection extends collection

trait collection:
  import compiletime.ops.int.*

  type Repeat[A, N <: Int] <: Tuple = N match
    case 0    => EmptyTuple
    case S[n] => A *: Repeat[A, n]

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
    inline def of[A: ClassTag](value: A, values: A*): NonEmptyArray[A] =
      apply[A].unsafe(value +: Array[A](values*))

    inline def apply[A]: RefinedTypeOps[Array[A], NonEmpty] =
      new RefinedTypeOps[Array[A], NonEmpty]

  object NonEmptyList:
    inline def of[A](value: A, values: A*): NonEmptyList[A] =
      apply[A].unsafe(value :: List[A](values*))

    inline def apply[A]: RefinedTypeOps[List[A], NonEmpty] =
      new RefinedTypeOps[List[A], NonEmpty]

  object NonEmptyVector:
    inline def of[A](value: A, values: A*): NonEmptyVector[A] =
      apply[A].unsafe(value +: Vector[A](values*))

    inline def apply[A]: RefinedTypeOps[Vector[A], NonEmpty] =
      new RefinedTypeOps[Vector[A], NonEmpty]

  object FixedArray:
    inline def of[A: ClassTag, N <: Int](values: Repeat[A, N]): FixedArray[A, N] =
      apply[A, N].unsafe(values.toList.asInstanceOf[List[A]].toArray)

    inline def apply[A, N <: Int]: RefinedTypeOps[Array[A], Size[N]] =
      new RefinedTypeOps[Array[A], Size[N]]

  object FixedList:
    inline def of[A, N <: Int](values: Repeat[A, N]): FixedList[A, N] =
      apply[A, N].unsafe(values.toList.asInstanceOf[List[A]])

    inline def apply[A, N <: Int]: RefinedTypeOps[List[A], Size[N]] =
      new RefinedTypeOps[List[A], Size[N]]

  object FixedVector:
    inline def of[A, N <: Int](values: Repeat[A, N]): FixedVector[A, N] =
      apply[A, N].unsafe(values.toList.toVector.asInstanceOf[Vector[A]])

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