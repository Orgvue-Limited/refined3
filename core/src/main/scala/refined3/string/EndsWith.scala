package refined3
package string

import quoted.{Expr => ExprQ, Quotes}

final case class EndsWith[S](s: S)

object EndsWith:
  transparent implicit inline def endsWithExpr[T, S]: Expr[T, EndsWith[S]] =
    Expr[T, EndsWith[S]]("\"" + stringOf[T] + "\".endsWith(\"" + stringOf[S] + "\")")

  transparent implicit inline def endsWithProof[T <: String, S]: Proof[T, EndsWith[S]] =
    inline if endsWith(stringOf[T], stringOf[S])
    then Proof.success[T, EndsWith[S]]
    else Proof.failure[T, EndsWith[S]]

  implicit inline def endsWithValidate[T <: String, S <: String]: Validate[T, EndsWith[S]] =
    (base: T) => base.endsWith(constValueOf[S])

  implicit inline def endsWithShow[T, S]: Show[T, EndsWith[S]] =
    (base: T) => s"\"$base\".endsWith(\"${stringOf[S]}\")"

  private transparent inline def endsWith(inline v: String, inline pred: String): Boolean =
    ${ endsWithCode('v, 'pred) }

  private def endsWithCode(v: ExprQ[String], pred: ExprQ[String])(using Quotes): ExprQ[Boolean] =
    val res = v.valueOrAbort.endsWith(pred.valueOrAbort)
    ExprQ(res)