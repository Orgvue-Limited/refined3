package refined.internal

import scala.quoted.*

object VectorMacros:
  transparent inline def vectorSize[T](inline in: Vector[T]): Int =
    ${ vectorSizeCode('in) }

  def vectorSizeCode[T: Type](in: Expr[Vector[T]])(using q: Quotes): Expr[Int] =
    import quotes.reflect.*

    def rec(tree: Term): Expr[Int] =
      tree match
        case Inlined(_, _, i) =>
          rec(i)
        case TypeApply(Select(Ident("Vector"), "empty"), _) =>
          Expr(0)
        case Apply(TypeApply(Select(Ident("Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case TypeApply(Select(Select(Ident("immutable"), "Vector"), "empty"), _) =>
          Expr(0)
        case Apply(TypeApply(Select(Select(Ident("immutable"), "Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "Vector"), "empty"), _) =>
          Expr(0)
        case Apply(TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "Vector"), "empty"), _) =>
          Expr(0)
        case Apply(TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case _ =>
          val treeStr = in.asTerm.show(using Printer.TreeStructure)
          q.reflect.report.errorAndAbort(s"Cannot determine size of Vector at compile time. Tree: $treeStr")

    rec(in.asTerm)

  transparent inline def vectorString[T](inline in: Vector[T]): String =
    ${ vectorStringCode('in) }

  def vectorStringCode[T: Type](in: Expr[Vector[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def transformTermToValue(tree: Term): String =
      tree match
        case Literal(lit) =>
          lit.value.toString()

        case _ =>
          "?"

    def rec(tree: Term): Expr[String] =
      tree match
        case Inlined(_, _, i) =>
          rec(i)
        case TypeApply(Select(Ident("Vector"), "empty"), _) =>
          Expr("Vector()")
        case Apply(TypeApply(Select(Ident("Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"Vector(${xs.map(transformTermToValue).mkString(", ")})")
        case TypeApply(Select(Select(Ident("immutable"), "Vector"), "empty"), _) =>
          Expr("Vector()")
        case Apply(TypeApply(Select(Select(Ident("immutable"), "Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"Vector(${xs.map(transformTermToValue).mkString(", ")})")
        case TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "Vector"), "empty"), _) =>
          Expr("Vector()")
        case Apply(TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"Vector(${xs.map(transformTermToValue).mkString(", ")})")
        case TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "Vector"), "empty"), _) =>
          Expr("Vector()")
        case Apply(TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "Vector"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"Vector(${xs.map(transformTermToValue).mkString(", ")})")
        case _ =>
          val treeStr = in.asTerm.show(using Printer.TreeStructure)
          q.reflect.report.errorAndAbort(s"Cannot determine size of Vector at compile time. Tree: $treeStr")

    rec(in.asTerm)