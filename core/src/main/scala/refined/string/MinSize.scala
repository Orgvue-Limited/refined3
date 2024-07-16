package refined
package string

import quoted.{Expr => ExprQ, Quotes}

final case class MinSize[N](n: N)

object MinSize:
  transparent implicit inline def minSizeExpr[T, N]: Expr[T, MinSize[N]] =
    Expr[T, MinSize[N]]("\"" + stringOf[T] + "\".length >= " + stringOf[N])

  transparent implicit inline def minSizeProof[T <: String, N <: Int]: Proof[T, MinSize[N]] =
    inline if length(stringOf[T]) >= constValueOf[N]
    then Proof.success[T, MinSize[N]]
    else Proof.failure[T, MinSize[N]]

  implicit inline def minSizeValidate[T <: String, N <: Int]: Validate[T, MinSize[N]] =
    (base: T) => base.length >= constValueOf[N]

  implicit inline def minSizeShow[T, N]: Show[T, MinSize[N]] =
    (base: T) => s"\"$base\".length >= ${stringOf[N]}"

  private transparent inline def length(inline v: String): Int =
    ${ lengthCode('v) }

  private def lengthCode(v: ExprQ[String])(using Quotes): ExprQ[Int] =
    val res = v.valueOrAbort.length()
    ExprQ(res)