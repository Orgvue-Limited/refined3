package refined
package collection

import refined.internal.HeadMacros

final case class Head[P](p: P)

object Head:
  // List
  transparent implicit inline def listHeadExpr[A, P](using inline list: List[A]): Expr[List[A], Head[P]] =
    Expr[List[A], Head[P]](HeadMacros.listExprStringHead[A, P](list))

  transparent implicit inline def nilExpr[P]: Expr[Nil.type, Head[P]] =
    Expr[Nil.type, Head[P]](HeadMacros.listExprStringHead[Nothing, P](Nil))

  transparent implicit inline def listHeadProof[A, P](using list: List[A]): Proof[List[A], Head[P]] =
    proof[List[A], P](HeadMacros.listHead[A, P](list))

  implicit def nilProof[P](using nil: Nil.type): Proof.Failure[Nil.type, Head[P]] =
    Proof.failure[Nil.type, Head[P]]

  // Array
  transparent implicit inline def arrayHeadExpr[A, P](using inline array: Array[A]): Expr[Array[A], Head[P]] =
    Expr[Array[A], Head[P]](HeadMacros.arrayExprStringHead[A, P](array))

  transparent implicit inline def arrayHeadProof[A, P](using array: Array[A]): Proof[Array[A], Head[P]] =
    proof[Array[A], P](HeadMacros.arrayHead[A, P](array))

  // Vector
  transparent implicit inline def vectorHeadExpr[A, P](using inline vector: Vector[A]): Expr[Vector[A], Head[P]] =
    Expr[Vector[A], Head[P]](HeadMacros.vectorExprStringHead[A, P](vector))

  transparent implicit inline def vectorHeadProof[A, P](using vector: Vector[A]): Proof[Vector[A], Head[P]] =
    proof[Vector[A], P](HeadMacros.vectorHead[A, P](vector))

  // Validate
  implicit inline def HeadValidate[I[_], A, P](using ev: I[A] => Iterable[A], validate: Validate[A, P]): Validate[I[A], Head[P]] =
    (base: I[A]) => ev(base).headOption.map(element => validate(element)).getOrElse(false)

  // Show
  implicit inline def sizeShow[I[_], A, P](using ev: I[A] => Iterable[A], show: Show[String, P]): Show[I[A], Head[P]] =
    (base: I[A]) => show(s"head(${ev(base)})")

  // Proof Helper
  private transparent inline def proof[T, P](inline bool: Boolean): Proof[T, Head[P]] =
    inline if bool
    then Proof.success[T, Head[P]]
    else Proof.failure[T, Head[P]]