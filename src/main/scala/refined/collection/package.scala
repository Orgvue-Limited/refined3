package refined

import refined.generic.Not

package object collection:
  type Empty    = Size[0]
  type NonEmpty = Not[Empty]

  implicit inline def nonEmptyArrayRefined[A](inline array: Array[A]): Array[A] Refined NonEmpty =
    Refined.refineCustom(array)(
      Not.notProof[Array[A], Empty](using Size.arrayProof[A, 0](array)),
      Not.notExpr[Array[A], Empty](using Size.arrayExpr[A, 0](array))
    )

  implicit inline def nonEmptyListRefined[A](inline list: List[A]): List[A] Refined NonEmpty =
    Refined.refineCustom(list)(
      Not.notProof[List[A], Empty](using Size.listProof[A, 0](list)),
      Not.notExpr[List[A], Empty](using Size.listExpr[A, 0](list))
    )

  implicit inline def nonEmptyVectorRefined[A](inline vector: Vector[A]): Vector[A] Refined NonEmpty =
    Refined.refineCustom(vector)(
      Not.notProof[Vector[A], Empty](using Size.vectorProof[A, 0](vector)),
      Not.notExpr[Vector[A], Empty](using Size.vectorExpr[A, 0](vector))
    )
