package refined.internal

import scala.quoted.*

object ListMacros:

  transparent inline def listSize[T](inline in: List[T]): Int =
    ${ listSizeCode('in) }

  def listSizeCode[T](in: Expr[List[T]])(using q: Quotes): Expr[Int] =
    import quotes.reflect.*

    def resolveIdent(ident: Ident): Option[Term] =
      ident.symbol.tree match
        case ValDef(_, _, Some(rhs))    => Some(rhs)
        case DefDef(_, _, _, Some(rhs)) => Some(rhs)
        case _                          => None

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot determine size of list in compile time. Tree: $treeStr")

    def rec(tree: Term): Expr[Int] =
      tree match
        case Inlined(_, _, i) =>
          rec(i)
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case TypeApply(Select(Ident("List"), "empty"), _) =>
          Expr(0)
        case Ident("Nil") =>
          Expr(0)
        case Apply(TypeApply(Select(Select(Ident("immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case TypeApply(Select(Select(Ident("immutable"), "List"), "empty"), _) =>
          Expr(0)
        case Select(Ident("immutable"), "Nil") =>
          Expr(0)
        case Apply(TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "List"), "empty"), _) =>
          Expr(0)
        case Select(Select(Ident("collection"), "immutable"), "Nil") =>
          Expr(0)
        case Apply(TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(xs.size)
        case TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "List"), "empty"), _) =>
          Expr(0)
        case Select(Select(Select(Ident("scala"), "collection"), "immutable"), "Nil") =>
          Expr(0)
        case ident @ Ident(_) =>
          resolveIdent(ident) match
            case None        =>
              failTree
            case Some(value) =>
              rec(value)
        case _ =>
          failTree

    rec(in.asTerm)

  transparent inline def listString[T](inline in: List[T]): String =
    ${ listStringCode('in) }

  def listStringCode[T](in: Expr[List[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot transform list to string in compile time. Tree: $treeStr")

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
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"List(${xs.map(transformTermToValue).mkString(", ")})")
        case TypeApply(Select(Ident("List"), "empty"), _) =>
          Expr("Nil")
        case Ident("Nil") =>
          Expr("Nil")
        case Apply(TypeApply(Select(Select(Ident("immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"List(${xs.map(transformTermToValue).mkString(", ")})")
        case TypeApply(Select(Select(Ident("immutable"), "List"), "empty"), _) =>
          Expr("Nil")
        case Select(Ident("immutable"), "Nil") =>
          Expr("Nil")
        case Apply(TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"List(${xs.map(transformTermToValue).mkString(", ")})")
        case TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "List"), "empty"), _) =>
          Expr("Nil")
        case Select(Select(Ident("collection"), "immutable"), "Nil") =>
          Expr("Nil")
        case Apply(TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          Expr(s"List(${xs.map(transformTermToValue).mkString(", ")})")
        case TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "List"), "empty"), _) =>
          Expr("Nil")
        case Select(Select(Select(Ident("scala"), "collection"), "immutable"), "Nil") =>
          Expr("Nil")
        case ident @ Ident(_) =>
          resolveIdent(ident) match
            case None        =>
              failTree
            case Some(value) =>
              rec(value)
        case _ =>
          failTree

    rec(in.asTerm)