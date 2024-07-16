package refined3

trait Show[T, P]:
  def apply(base: T): String