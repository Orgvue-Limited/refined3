package refined3

trait Coercible[A, B]:
  inline final def apply(a: A): B = a.asInstanceOf[B]

object Coercible:
  def apply[A, B](implicit ev: Coercible[A, B]): Coercible[A, B] = ev

  def instance[A, B]: Coercible[A, B] = _instance.asInstanceOf[Coercible[A, B]]

  private val _instance = new Coercible[Any, Any] {}

  extension [A] (repr: A)
    inline def coerce[B](implicit ev: Coercible[A, B]): B = repr.asInstanceOf[B]

  given refinedToValue[T, P]: Coercible[T Refined P, T] =
    Coercible.instance