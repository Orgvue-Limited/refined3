package refined3
package numeric

import refined3.internal.WitnessAs

final case class Modulo[N, O](n: N, o: O)

object Modulo:
  transparent implicit inline def moduloExpr[T, N, O]: Expr[T, Modulo[N, O]] =
    Expr[T, Modulo[N, O]](stringOf[T] + " % " + stringOf[N] + " == " + stringOf[O])

  transparent implicit inline def moduloProof[T, N, O](using
    T: WitnessAs[T, Long],
    N: WitnessAs[N, Long],
    O: WitnessAs[O, Long]
  ): Proof[T, Modulo[N, O]] =
    inline if T.value % N.value == O.value
    then Proof.success[T, Modulo[N, O]]
    else Proof.failure[T, Modulo[N, O]]

  implicit inline def moduloValidate[T, N, O](using
    N: Integral[T],
    W: WitnessAs[N, T],
    O: WitnessAs[O, T]
  ): Validate[T, Modulo[N, O]] =
    (base: T) => N.rem(base, W.value) == O.value

  implicit inline def moduloShow[T, N, O]: Show[T, Modulo[N, O]] =
    (base: T) => s"$base % ${valueOf[N]} == ${valueOf[O]}"