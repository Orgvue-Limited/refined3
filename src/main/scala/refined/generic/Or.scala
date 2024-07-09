package refined
package generic

final infix case class Or[P0, P1](first: P0, second: P1)

object Or:
  transparent implicit inline def orExpr[T, P0, P1](using
    inline expr0: Expr[T, P0],
    inline expr1: Expr[T, P1]
  ): Expr[T, P0 Or P1] =
    Expr[T, P0 Or P1](expr0.value + " || " + expr1.value)

  transparent implicit inline def orProof[T, P0, P1](using
    proof0: Proof[T, P0],
    proof1: Proof[T, P1]
  ): Proof[T, P0 Or P1] =
    inline (proof0, proof1) match
      case (_: Proof.Successful[?, ?], _) =>
        Proof.success[T, P0 Or P1]
      case (_, _: Proof.Successful[?, ?]) =>
        Proof.success[T, P0 Or P1]
      case _                              =>
        Proof.failure[T, P0 Or P1]

  implicit inline def orValidate[T, P0, P1](using
    validate0: Validate[T, P0],
    validate1: Validate[T, P1]
  ): Validate[T, P0 Or P1] =
    (base: T) => validate0(base) || validate1(base)

  implicit inline def orShow[T, P0, P1](using
    show0: Show[T, P0],
    show1: Show[T, P1]
  ): Show[T, P0 Or P1] =
    (base: T) => s"${show0(base)} || ${show1(base)}"