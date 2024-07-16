package refined3
package string

import quoted.{Expr => ExprQ, Quotes}

final case class MaxSize[N](n: N)

object MaxSize:
  transparent implicit inline def maxSizeExpr[T, N]: Expr[T, MaxSize[N]] =
    Expr[T, MaxSize[N]]("\"" + stringOf[T] + "\".length <= " + stringOf[N])

  transparent implicit inline def maxSizeProof[T <: String, N <: Int]: Proof[T, MaxSize[N]] =
    inline if length(stringOf[T]) <= constValueOf[N]
    then Proof.success[T, MaxSize[N]]
    else Proof.failure[T, MaxSize[N]]

  implicit inline def maxSizeValidate[T <: String, N <: Int]: Validate[T, MaxSize[N]] =
    (base: T) => base.length <= constValueOf[N]

  implicit inline def maxSizeShow[T, N]: Show[T, MaxSize[N]] =
    (base: T) => s"\"$base\".length <= ${stringOf[N]}"

  private transparent inline def length(inline v: String): Int =
    ${ lengthCode('v) }

  private def lengthCode(v: ExprQ[String])(using Quotes): ExprQ[Int] =
    val res = v.valueOrAbort.length()
    ExprQ(res)