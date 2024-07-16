package refined3
package string

import quoted.{Expr => ExprQ, Quotes}

final case class StartsWith[S](s: S)

object StartsWith:
  transparent implicit inline def startsWithExpr[T, S]: Expr[T, StartsWith[S]] =
    Expr[T, StartsWith[S]]("\"" + stringOf[T] + "\".startsWith(\"" + stringOf[S] + "\")")

  transparent implicit inline def startsWithProof[T <: String, S]: Proof[T, StartsWith[S]] =
    inline if startsWith(stringOf[T], stringOf[S])
    then Proof.success[T, StartsWith[S]]
    else Proof.failure[T, StartsWith[S]]

  implicit inline def startsWithValidate[T <: String, S <: String]: Validate[T, StartsWith[S]] =
    (base: T) => base.startsWith(constValueOf[S])

  implicit inline def startsWithShow[T, S]: Show[T, StartsWith[S]] =
    (base: T) => s"\"$base\".startsWith(\"${stringOf[S]}\")"

  private transparent inline def startsWith(inline v: String, inline pred: String): Boolean =
    ${ startsWithCode('v, 'pred) }

  private def startsWithCode(v: ExprQ[String], pred: ExprQ[String])(using Quotes): ExprQ[Boolean] =
    val res = v.valueOrAbort.startsWith(pred.valueOrAbort)
    ExprQ(res)