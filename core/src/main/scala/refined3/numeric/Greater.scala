package refined3
package numeric

import refined3.internal.WitnessAs

final case class Greater[N](n: N)

object Greater:
  transparent implicit inline def greaterExpr[T, N]: Expr[T, Greater[N]] =
    Expr[T, Greater[N]](stringOf[T] + " > " + stringOf[N])

  transparent implicit inline def greaterProof[T, N](using T: WitnessAs[T, Long], N: WitnessAs[N, Long]): Proof[T, Greater[N]] =
    inline if T.value > N.value
    then Proof.success[T, Greater[N]]
    else Proof.failure[T, Greater[N]]

  implicit inline def greaterValidate[T, N](using N: Numeric[T], W: WitnessAs[N, T]): Validate[T, Greater[N]] =
    (base: T) => N.gt(base, W.value)

  implicit inline def greaterShow[T, N]: Show[T, Greater[N]] =
    (base: T) => s"$base > ${valueOf[N]}"