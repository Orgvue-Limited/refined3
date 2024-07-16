package refined3

trait Validate[T, P]:
  def apply(base: T): Boolean