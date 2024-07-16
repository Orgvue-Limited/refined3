package refined
package collection

import refined.internal.{ArrayMacros, ListMacros, VectorMacros, WitnessAs}

final case class MaxSize[N](n: N)

object MaxSize:
  // List
  implicit inline def listExpr[A, N <: Int](using inline list: List[A]): Expr[List[A], MaxSize[N]] =
    expr[List[A], N](ListMacros.listString[A](list))

  implicit inline def nilExpr[N <: Int]: Expr[Nil.type, MaxSize[N]] =
    expr[Nil.type, N](ListMacros.listString[Nothing](Nil))

  transparent implicit inline def listProof[A, N <: Int](using list: List[A]): Proof[List[A], MaxSize[N]] =
    proof[List[A], N](ListMacros.listSize[A](list))

  transparent implicit inline def nilProof[N <: Int](using nil: Nil.type): Proof[Nil.type, MaxSize[N]] =
    proof[Nil.type, N](0)

  // Array
  implicit inline def arrayExpr[A, N <: Int](using inline array: Array[A]): Expr[Array[A], MaxSize[N]] =
    expr[Array[A], N](ArrayMacros.arrayString[A](array))

  transparent implicit inline def arrayProof[A, N <: Int](using array: Array[A]): Proof[Array[A], MaxSize[N]] =
    proof[Array[A], N](ArrayMacros.arraySize[A](array))

  // Vector
  implicit inline def vectorExpr[A, N <: Int](using inline vector: Vector[A]): Expr[Vector[A], MaxSize[N]] =
    expr[Vector[A], N](VectorMacros.vectorString[A](vector))

  transparent implicit inline def vectorProof[A, N <: Int](using vector: Vector[A]): Proof[Vector[A], MaxSize[N]] =
    proof[Vector[A], N](VectorMacros.vectorSize[A](vector))

  // Validate
  implicit inline def sizeValidate[T, N](using N: WitnessAs[N, Long], ev: T => Iterable[?]): Validate[T, MaxSize[N]] =
    (base: T) => ev(base).size <= N.value

  // Show
  implicit inline def sizeShow[T, N](using N: WitnessAs[N, Long]): Show[T, MaxSize[N]] =
    (base: T) => s"size(${base}) <= ${N.value}"

  // Helpers
  private transparent inline def proof[T, N <: Int](inline size: Int): Proof[T, MaxSize[N]] =
    inline if size <= constValueOf[N]
    then Proof.success[T, MaxSize[N]]
    else Proof.failure[T, MaxSize[N]]

  private transparent inline def expr[T, N <: Int](inline expr: String): Expr[T, MaxSize[N]] =
    Expr[T, MaxSize[N]]("size(" + expr + ") <= " + stringOf[N])