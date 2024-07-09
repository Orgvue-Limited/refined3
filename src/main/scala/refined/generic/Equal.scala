package refined
package generic

final case class Equal[U](u: U)

object Equal:
  transparent implicit inline def equalShow[T, U]: Show[T, Equal[U]] =
    Show[T, Equal[U]](stringOf[T] + " == " + stringOf[U])

  transparent implicit inline def equalProof[T, U]: Proof[T, Equal[U]] =
    inline if constValueOf[T] == constValueOf[U]
    then Proof.success[T, Equal[U]]
    else Proof.failure[T, Equal[U]]

  implicit inline def equalValidate[T, U]: Validate[T, Equal[U]] =
    (base: T) =>
      val predicate = valueOf[U]
      if base == predicate then null
      else s"$base == $predicate"