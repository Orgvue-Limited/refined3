package refined3
package string

import quoted.{Expr => ExprQ, Quotes}

final case class MatchesRegex[S](s: S)

object MatchesRegex:
  transparent implicit inline def matchesRegexExpr[T, S]: Expr[T, MatchesRegex[S]] =
    Expr[T, MatchesRegex[S]]("\"" + stringOf[T] + "\".matches(\"" + stringOf[S] + "\")")

  transparent implicit inline def matchesRegexProof[T <: String, S]: Proof[T, MatchesRegex[S]] =
    inline if matches(stringOf[T], stringOf[S])
    then Proof.success[T, MatchesRegex[S]]
    else Proof.failure[T, MatchesRegex[S]]

  implicit inline def matchesRegexValidate[T <: String, S <: String]: Validate[T, MatchesRegex[S]] =
    (base: T) => base.matches(constValueOf[S])

  implicit inline def matchesRegexShow[T, S]: Show[T, MatchesRegex[S]] =
    (base: T) => s"\"$base\".matches(\"${stringOf[S]}\")"

  private transparent inline def matches(inline v: String, inline pred: String): Boolean =
    ${ matchesCode('v, 'pred) }

  private def matchesCode(v: ExprQ[String], pred: ExprQ[String])(using Quotes): ExprQ[Boolean] =
    val res = v.valueOrAbort.matches(pred.valueOrAbort)
    ExprQ(res)