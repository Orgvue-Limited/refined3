package refined
package numeric

import refined.internal.WitnessAs

final case class Less[N](n: N)

object Less:
  transparent implicit inline def lessExpr[T, N]: Expr[T, Less[N]] =
    Expr[T, Less[N]](stringOf[T] + " < " + stringOf[N])

  transparent implicit inline def lessProof[T, N](using T: WitnessAs[T, Long], N: WitnessAs[N, Long]): Proof[T, Less[N]] =
    inline if T.value < N.value
    then Proof.success[T, Less[N]]
    else Proof.failure[T, Less[N]]

  implicit inline def lessValidate[T, N](using N: Numeric[T], ev: N <:< T): Validate[T, Less[N]] =
    (base: T) => N.lt(base, ev(valueOf[N]))

  implicit inline def lessShow[T, N]: Show[T, Less[N]] =
    (base: T) => s"$base < ${valueOf[N]}"