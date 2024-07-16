package refined
package internal

import scala.compiletime.{constValue, error}

import scala.compiletime.ops.{double, int, float, long}

final case class WitnessAs[A, +B](value: B)

object WitnessAs:
  inline def apply[A, B](implicit ev: WitnessAs[A, B]): WitnessAs[A, B] =
    ev

  transparent inline given singletonIntWitnessAs[A <: Int]: WitnessAs[A, A] =
    inline val a = constValue[A]
    WitnessAs[A, A](a)

  transparent inline given singletonLongWitnessAs[A <: Long]: WitnessAs[A, A] =
    inline val a = constValue[A]
    WitnessAs[A, A](a)

  transparent inline given longWitnessAsInt[A <: Long]: WitnessAs[A, long.ToInt[A]] =
    inline constValue[A] match
      case a if a >= Int.MinValue && a <= Int.MaxValue =>
        WitnessAs[A, long.ToInt[A]](constValue[long.ToInt[A]])
      case a =>
        error(s"WitnessAs: $a is not in [Int.MinValue, Int.MaxValue]")

  transparent inline given floatWitnessAsInt[A <: Float]: WitnessAs[A, float.ToInt[A]] =
    inline constValue[A] match
      case a if a >= Int.MinValue && a <= Int.MaxValue =>
        WitnessAs[A, float.ToInt[A]](constValue[float.ToInt[A]])
      case a =>
        error(s"WitnessAs: $a is not in [Int.MinValue, Int.MaxValue]")

  transparent inline given doubleWitnessAsInt[A <: Double]: WitnessAs[A, double.ToInt[A]] =
    inline constValue[A] match
      case a if a >= Int.MinValue && a <= Int.MaxValue =>
        WitnessAs[A, double.ToInt[A]](constValue[double.ToInt[A]])
      case a =>
        error(s"WitnessAs: $a is not in [Int.MinValue, Int  .MaxValue]")

  transparent inline given intWitnessAsLong[A <: Int]: WitnessAs[A, int.ToLong[A]] =
    inline val a = constValue[int.ToLong[A]]
    WitnessAs(a)

  transparent inline given intWitnessAsFloat[A <: Int]: WitnessAs[A, int.ToFloat[A]] =
    inline val a = constValue[int.ToFloat[A]]
    WitnessAs(a)

  transparent inline given intWitnessAsDouble[A <: Int]: WitnessAs[A, int.ToDouble[A]] =
    inline val a = constValue[int.ToDouble[A]]
    WitnessAs(a)

  transparent inline given floatWitnessAsLong[A <: Float]: WitnessAs[A, float.ToLong[A]] =
    inline constValue[A] match
      case a if a >= Long.MinValue && a <= Long.MaxValue =>
        WitnessAs[A, float.ToLong[A]](constValue[float.ToLong[A]])
      case a =>
        error(s"WitnessAs: $a is not in [Long.MinValue, Long.MaxValue]")

  transparent inline given doubleWitnessAsLong[A <: Double]: WitnessAs[A, double.ToLong[A]] =
    inline constValue[A] match
      case a if a >= Long.MinValue && a <= Long.MaxValue =>
        WitnessAs[A, double.ToLong[A]](constValue[double.ToLong[A]])
      case a =>
        error(s"WitnessAs: $a is not in [Long.MinValue, Long.MaxValue]")