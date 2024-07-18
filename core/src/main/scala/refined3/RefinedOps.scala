package refined3

trait RefinedOps[T, P](using validate: Validate[T, P], show: Show[T, P]):
  def from(value: T): Either[String, Refined[T, P]] =
    Refined.refineV(value)(using validate, show)

  def unapply(t: T): Option[Refined[T, P]] =
    from(t).toOption

  def unsafe(value: T): Refined[T, P] =
    Refined.unsafe[T, P](value)

class RefinedPrimitiveOps[T, P](using validate: Validate[T, P], show: Show[T, P]) extends RefinedOps[T, P]:
  inline def apply[S <: Singleton](value: S)(using
    ev: S <:< T,
    inline proof: Proof[S, P],
    inline expr: refined3.Expr[S, P]
  ): Refined[S, P] =
    Refined.refineMV[S, P](value)(using proof, expr)

class RefinedTypeOps[T, P](using validate: Validate[T, P], show: Show[T, P]) extends RefinedOps[T, P]:
  inline def apply(inline value: T): Refined[T, P] =
    Refined.refineM[T, P](value)