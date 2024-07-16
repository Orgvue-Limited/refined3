package refined3
package string

import quoted.{Expr => ExprQ, Quotes}

final case class Empty()

object Empty:
  transparent implicit inline def emptyExpr[T]: Expr[T, Empty] =
    Expr[T, Empty]("\"" + stringOf[T] + "\".isEmpty")

  transparent implicit inline def emptyProof[T <: String]: Proof[T, Empty] =
    inline if empty(stringOf[T])
    then Proof.success[T, Empty]
    else Proof.failure[T, Empty]

  implicit inline def emptyValidate[T <: String]: Validate[T, Empty] =
    (base: T) => base.isEmpty()

  implicit inline def emptyShow[T, S]: Show[T, Empty] =
    (base: T) => s"\"$base\".isEmpty"

  private transparent inline def empty(inline v: String): Boolean =
    ${ emptyCode('v) }

  private def emptyCode(v: ExprQ[String])(using Quotes): ExprQ[Boolean] =
    val res = v.valueOrAbort.isEmpty()
    ExprQ(res)