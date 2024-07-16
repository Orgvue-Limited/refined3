package refined

trait Show[T, P]:
  def apply(base: T): String