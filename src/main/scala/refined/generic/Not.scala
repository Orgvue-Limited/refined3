package refined
package generic

final case class Not[P](p: P)

object Not:
  transparent implicit inline def notShow[T, P](using inline show: Show[T, P]): Show[T, Not[P]] =
    Show[T, Not[P]]("Not (" + show.expr + ")")

  transparent implicit inline def notProof[T, P](using proof: Proof[T, P]): Proof[T, Not[P]] =
    inline proof match
      case _: Proof.Successful[T, P] => Proof.failure[T, Not[P]]
      case _                         => Proof.success[T, Not[P]]

  implicit inline def equalValidate[T, P](using validate: Validate[T, P]): Validate[T, Not[P]] =
    (base: T) =>
      validate(base) match
        case null => "Negation"
        case msg  => null