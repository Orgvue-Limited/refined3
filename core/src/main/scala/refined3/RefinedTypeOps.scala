package refined3

class RefinedTypeOps[T, P](using validate: Validate[T, P], show: Show[T, P]) {
  def from(value: T): Either[String, Refined[T, P]] =
    Refined.refineV(value)(using validate, show)

  def unapply(t: T): Option[Refined[T, P]] =
    from(t).toOption

  def unsafe(value: T): Refined[T, P] =
    Refined.unsafe[T, P](value)
}