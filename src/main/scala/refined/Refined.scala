package refined

import scala.quoted.*

infix opaque type Refined[+T, P] = T

object Refined:
  inline def unsafe[T, P](value: T): T Refined P = value

  inline implicit def unwrap[T, P](in: Refined[T, P]): T = in

  def unapply[T, P](r: T Refined P): Some[T] = Some(r)

  extension[T, P] (refined: T Refined P )
    transparent inline def value: T = refined

  transparent inline def refineV[T, P](value: T)(using validate: Validate[T, P]): Either[String, T Refined P] =
    validate(value) match
      case null =>
        Right(Refined.unsafe[T, P](value))
      case msg  =>
        Left(s"Validation failed: $msg")

  implicit inline def refineMV[T <: Singleton, P](value: T)(using inline proof: Proof[T, P], inline show: Show[T, P]): T Refined P =
    inline proof match 
      case _: Proof.Successful[?, ?] =>
        Refined.unsafe[T, P](value)
      case _                         =>
        reportError("Validation failed: " + show.expr)

  private transparent inline def reportError(a: String): Nothing = ${ reportErrorCode('a) }

  private def reportErrorCode(a: Expr[String])(using q: Quotes): Nothing =
    q.reflect.report.errorAndAbort(a.valueOrAbort)