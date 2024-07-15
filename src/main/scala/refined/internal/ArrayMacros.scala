package refined.internal

import scala.quoted.*

object ArrayMacros:
  transparent inline def arraySize[T](inline in: Array[T]): Int =
    ${ arraySizeCode('in) }

  def arraySizeCode[T: Type](in: Expr[Array[T]])(using q: Quotes): Expr[Int] =
    import quotes.reflect.*

    def resolveIdent(ident: Ident): Option[Term] =
      ident.symbol.tree match
        case ValDef(_, _, Some(rhs))    => Some(rhs)
        case DefDef(_, _, _, Some(rhs)) => Some(rhs)
        case _                          => None

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
          q.reflect.report.errorAndAbort(s"Cannot determine size of array at compile time. Tree: $treeStr")

    def rec(tree: Term): Expr[Int] =
      tree match
        case Inlined(_, _, i) =>
          rec(i)
        case Apply(TypeApply(Select(Ident("Array"), "empty"), _), _) =>
          Expr(0)
        case Apply(Select(Ident("Array"), "apply"), List(Literal(_), Typed(Repeated(xs, _), _))) =>
          Expr(1 + xs.size)
        case Apply(Select(Ident("Array"), "apply"), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case Apply(Apply(TypeApply(Select(Ident("Array"), "apply"), _), List(Typed(Repeated(xs, _), _))), _) =>
          Expr(xs.size)
        case Apply(TypeApply(Select(Select(Ident("scala"), "Array"), "empty"), _), _) =>
          Expr(0)
        case Apply(Select(Select(Ident("scala"), "Array"), "apply"), List(Literal(_), Typed(Repeated(xs, _), _))) =>
          Expr(1 + xs.size)
        case Apply(Select(Select(Ident("scala"), "Array"), "apply"), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case Apply(Apply(TypeApply(Select(Select(Ident("scala"), "Array"), "apply"), _), List(Typed(Repeated(xs, _), _))), _) =>
          Expr(xs.size)
        case ident @ Ident(_) =>
          resolveIdent(ident) match
            case None        =>
              failTree
            case Some(value) =>
              rec(value)
        case _ =>
          failTree

    rec(in.asTerm)

  transparent inline def arrayString[T](inline in: Array[T]): String =
    ${ arrayStringCode('in) }

  def arrayStringCode[T: Type](in: Expr[Array[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot transform array to string in compile time. Tree: $treeStr")

    def transformTermToValue(tree: Term): String =
      tree match
        case Literal(lit) =>
          lit.value.toString()

        case _ =>
          "?"

    def resolveIdent(ident: Ident): Option[Term] =
      ident.symbol.tree match
        case ValDef(_, _, Some(rhs))    => Some(rhs)
        case DefDef(_, _, _, Some(rhs)) => Some(rhs)
        case _                          => None

    def rec(tree: Term): Expr[String] =
      tree match
        case Inlined(_, _, i) =>
          rec(i)
        case Apply(TypeApply(Select(Ident("Array"), "empty"), _), _) =>
          Expr("Array()")
        case Apply(Select(Ident("Array"), "apply"), List(lit @ Literal(_), Typed(Repeated(xs, _), _))) =>
          Expr(s"Array(${transformTermToValue(lit)}, ${xs.map(transformTermToValue).mkString(", ")})")
        case Apply(Select(Ident("Array"), "apply"), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"Array(${xs.map(transformTermToValue).mkString(", ")})")
        case Apply(Apply(TypeApply(Select(Ident("Array"), "apply"), _), List(Typed(Repeated(xs, _), _))), _) =>
          Expr(s"Array(${xs.map(transformTermToValue).mkString(", ")})")
        case Apply(TypeApply(Select(Select(Ident("scala"), "Array"), "empty"), _), _) =>
          Expr("Array()")
        case Apply(Select(Select(Ident("scala"), "Array"), "apply"), List(lit @ Literal(_), Typed(Repeated(xs, _), _))) =>
           Expr(s"Array(${transformTermToValue(lit)}, ${xs.map(transformTermToValue).mkString(", ")})")
        case Apply(Select(Select(Ident("scala"), "Array"), "apply"), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"Array(${xs.map(transformTermToValue).mkString(", ")})")
        case Apply(Apply(TypeApply(Select(Select(Ident("scala"), "Array"), "apply"), _), List(Typed(Repeated(xs, _), _))), _) =>
          Expr(s"Array(${xs.map(transformTermToValue).mkString(", ")})")
        case ident @ Ident(_) =>
          resolveIdent(ident) match
            case None        =>
              failTree
            case Some(value) =>
              rec(value)
        case _ =>
          failTree

    rec(in.asTerm)