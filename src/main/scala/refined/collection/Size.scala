package refined
package collection

import refined.internal.{ArrayMacros, ListMacros, VectorMacros, WitnessAs}

final case class Size[N](n: N)

object Size:
  // List
  implicit inline def listExpr[A, N <: Int](inline list: List[A]): Expr[List[A], Size[N]] =
    expr[List[A], N](ListMacros.listString[A](list))

  transparent implicit inline def listProof[A, N <: Int](inline list: List[A]): Proof[List[A], Size[N]] =
    proof[List[A], N](ListMacros.listSize[A](list))

  implicit inline def listRefineSize[A, N <: Int](inline list: List[A]): List[A] Refined Size[N] =
    Refined.refineCustom(list)(Size.listProof[A, N](list), Size.listExpr[A, N](list))

  // Array
  implicit inline def arrayExpr[A, N <: Int](inline array: Array[A]): Expr[Array[A], Size[N]] =
    expr[Array[A], N](ArrayMacros.arrayString[A](array))

  transparent implicit inline def arrayProof[A, N <: Int](inline array: Array[A]): Proof[Array[A], Size[N]] =
    proof[Array[A], N](ArrayMacros.arraySize[A](array))

  implicit inline def arayRefineSize[A, N <: Int](inline array: Array[A]): Array[A] Refined Size[N] =
    Refined.refineCustom(array)(Size.arrayProof[A, N](array), Size.arrayExpr[A, N](array))

  // Vector 
  implicit inline def vectorExpr[A, N <: Int](inline vector: Vector[A]): Expr[Vector[A], Size[N]] =
    expr[Vector[A], N](VectorMacros.vectorString[A](vector))

  transparent implicit inline def vectorProof[A, N <: Int](inline vector: Vector[A]): Proof[Vector[A], Size[N]] =
    proof[Vector[A], N](VectorMacros.vectorSize[A](vector))

  implicit inline def vectorRefineSize[A, N <: Int](inline vector: Vector[A]): Vector[A] Refined Size[N] =
    Refined.refineCustom(vector)(Size.vectorProof[A, N](vector), Size.vectorExpr[A, N](vector))

  // Validate
  implicit inline def sizeValidate[T, N](using N: WitnessAs[N, Long], ev: T => Iterable[?]): Validate[T, Size[N]] =
    (base: T) => ev(base).size == N.value

  // Show
  implicit inline def sizeShow[T, N](using N: WitnessAs[N, Long]): Show[T, Size[N]] =
    (base: T) => s"size(${base}) == ${N.value}"

  // Helpers
  private transparent inline def proof[T, N <: Int](inline size: Int): Proof[T, Size[N]] =
    inline if size == constValueOf[N]
    then Proof.success[T, Size[N]]
    else Proof.failure[T, Size[N]]

  private transparent inline def expr[T, N <: Int](inline expr: String): Expr[T, Size[N]] =
    Expr[T, Size[N]]("size(" + expr + ") == " + stringOf[N])