package refined3

import scala.quoted.*

import scala.compiletime.summonInline

infix opaque type Refined[+T, P] = T

object Refined:
  inline def apply[T, P](using refined: Refined[T, P]): Refined[T, P] = refined

  inline def unsafe[T, P](value: T): T Refined P = value

  inline implicit def unwrap[T, P](in: Refined[T, P]): T = in

  def unapply[T, P](r: T Refined P): Some[T] = Some(r)

  extension[T, P] (refined: T Refined P)
    transparent inline def value: T = refined

  inline def refineOption[T, P](value: T)(using validate: Validate[T, P]): Option[T Refined P] =
    if validate(value)
    then Some(Refined.unsafe[T, P](value))
    else None

  transparent inline def refineV[T, P](value: T)(using
    validate: Validate[T, P],
    show: Show[T, P]
  ): Either[String, T Refined P] =
    if validate(value)
    then Right(Refined.unsafe[T, P](value))
    else Left(s"Validation failed: ${show(value)}")

  implicit inline def refineMV[T <: Singleton, P](value: T)(using
    inline proof: Proof[T, P],
    inline expr: refined3.Expr[T, P]
  ): T Refined P =
    refineCustom[T, P](value)(proof, expr)

  implicit inline def refineM[T, P](inline value: T): T Refined P =
    implicit def implicitOfValue: T = value
    refineCustom[T, P](value)(summonInline[Proof[T, P]], summonInline[refined3.Expr[T, P]])

  private[refined3] transparent inline def refineCustom[T, P](value: T)(
    inline proof: Proof[T, P],
    inline expr: refined3.Expr[T, P]
  ): T Refined P =
    inline proof match 
      case _: Proof.Successful[?, ?] =>
        Refined.unsafe[T, P](value)
      case _                         =>
        reportError("Validation failed: " + expr.value)

  private[refined3] transparent inline def reportError(a: String): Nothing = ${ reportErrorCode('a) }

  private def reportErrorCode(a: Expr[String])(using q: Quotes): Nothing =
    q.reflect.report.errorAndAbort(a.valueOrAbort)