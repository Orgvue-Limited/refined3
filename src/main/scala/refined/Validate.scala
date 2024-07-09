package refined

trait Validate[T, P]:
  def apply(base: T): Boolean