package refined3
package generic

final case class Not[P](p: P)

object Not:
  transparent implicit inline def notExpr[T, P](using inline expr: Expr[T, P]): Expr[T, Not[P]] =
    Expr[T, Not[P]]("Not (" + expr.value + ")")

  transparent implicit inline def notProof[T, P](using proof: Proof[T, P]): Proof[T, Not[P]] =
    inline proof match
      case _: Proof.Successful[T, P] => Proof.failure[T, Not[P]]
      case _                         => Proof.success[T, Not[P]]

  implicit inline def notValidate[T, P](using validate: Validate[T, P]): Validate[T, Not[P]] =
    (base: T) => !validate(base)

  implicit inline def notShow[T, P](using show: Show[T, P]): Show[T, Not[P]] =
    (base: T) => s"Not (${show(base)})"