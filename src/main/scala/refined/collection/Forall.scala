package refined
package collection

import refined.internal.ForallMacros

final case class Forall[P](p: P)

object Forall:
  // List
  transparent implicit inline def listForallExpr[A, P](using list: List[A]): Expr[List[A], Forall[P]] =
    Expr[List[A], Forall[P]](ForallMacros.listExprStringForAll[A, P](list))

  implicit inline def nilExpr[P]: Expr[Nil.type, Forall[P]] =
    Expr[Nil.type, Forall[P]]("")

  transparent implicit inline def listForallProof[A, P](using list: List[A]): Proof[List[A], Forall[P]] =
    proof[List[A], P](ForallMacros.listForAll[A, P](list))

  implicit def nilProof[P](using nil: Nil.type): Proof.Successful[Nil.type, Forall[P]] =
    Proof.success[Nil.type, Forall[P]]

  // Array
  transparent implicit inline def arrayForallExpr[A, P](using array: Array[A]): Expr[Array[A], Forall[P]] =
    Expr[Array[A], Forall[P]](ForallMacros.arrayExprStringForAll[A, P](array))

  transparent implicit inline def arrayForallProof[A, P](using array: Array[A]): Proof[Array[A], Forall[P]] =
    proof[Array[A], P](ForallMacros.arrayForAll[A, P](array))

  // Vector
  transparent implicit inline def vectorForallExpr[A, P](using vector: Vector[A]): Expr[Vector[A], Forall[P]] =
    Expr[Vector[A], Forall[P]](ForallMacros.vectorExprStringForAll[A, P](vector))

  transparent implicit inline def vectorForallProof[A, P](using vector: Vector[A]): Proof[Vector[A], Forall[P]] =
    proof[Vector[A], P](ForallMacros.vectorForAll[A, P](vector))

  // Validate
  implicit inline def forallValidate[I[_], A, P](using ev: I[A] => Iterable[A], validate: Validate[A, P]): Validate[I[A], Forall[P]] =
    (base: I[A]) => ev(base).forall(element => validate(element))

  // Show
  implicit inline def sizeShow[I[_], A, P](using ev: I[A] => Iterable[A], show: Show[A, P]): Show[I[A], Forall[P]] =
    (base: I[A]) => ev(base).map(show(_)).mkString(" && ")

  // Proof Helper
  private transparent inline def proof[T, P](inline bool: Boolean): Proof[T, Forall[P]] =
    inline if bool
    then Proof.success[T, Forall[P]]
    else Proof.failure[T, Forall[P]]