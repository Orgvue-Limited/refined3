package refined3
package collection

import refined3.internal.{ArrayMacros, ListMacros, VectorMacros, WitnessAs}

final case class MinSize[N](n: N)

object MinSize:
  // List
  implicit inline def listExpr[A, N <: Int](using inline list: List[A]): Expr[List[A], MinSize[N]] =
    expr[List[A], N](ListMacros.listString[A](list))

  implicit inline def nilExpr[N <: Int]: Expr[Nil.type, MinSize[N]] =
    expr[Nil.type, N](ListMacros.listString[Nothing](Nil))

  transparent implicit inline def listProof[A, N <: Int](using list: List[A]): Proof[List[A], MinSize[N]] =
    proof[List[A], N](ListMacros.listSize[A](list))

  transparent implicit inline def nilProof[N <: Int](using nil: Nil.type): Proof[Nil.type, MinSize[N]] =
    proof[Nil.type, N](0)

  // Array
  implicit inline def arrayExpr[A, N <: Int](using inline array: Array[A]): Expr[Array[A], MinSize[N]] =
    expr[Array[A], N](ArrayMacros.arrayString[A](array))

  transparent implicit inline def arrayProof[A, N <: Int](using array: Array[A]): Proof[Array[A], MinSize[N]] =
    proof[Array[A], N](ArrayMacros.arraySize[A](array))

  // Vector
  implicit inline def vectorExpr[A, N <: Int](using inline vector: Vector[A]): Expr[Vector[A], MinSize[N]] =
    expr[Vector[A], N](VectorMacros.vectorString[A](vector))

  transparent implicit inline def vectorProof[A, N <: Int](using vector: Vector[A]): Proof[Vector[A], MinSize[N]] =
    proof[Vector[A], N](VectorMacros.vectorSize[A](vector))

  // Validate
  implicit inline def sizeValidate[T, N](using N: WitnessAs[N, Long], ev: T => Iterable[?]): Validate[T, MinSize[N]] =
    (base: T) => ev(base).size >= N.value

  // Show
  implicit inline def sizeShow[T, N](using N: WitnessAs[N, Long]): Show[T, MinSize[N]] =
    (base: T) => s"size(${base}) >= ${N.value}"

  // Helpers
  private transparent inline def proof[T, N <: Int](inline size: Int): Proof[T, MinSize[N]] =
    inline if size >= constValueOf[N]
    then Proof.success[T, MinSize[N]]
    else Proof.failure[T, MinSize[N]]

  private transparent inline def expr[T, N <: Int](inline expr: String): Expr[T, MinSize[N]] =
    Expr[T, MinSize[N]]("size(" + expr + ") >= " + stringOf[N])