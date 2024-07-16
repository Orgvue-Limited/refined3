package refined.internal

import scala.quoted.*

object ListMacros:

  transparent inline def listSize[T](inline in: List[T]): Int =
    ${ listSizeCode('in) }

  def listSizeCode[T](in: Expr[List[T]])(using q: Quotes): Expr[Int] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot determine size of list in compile time. Tree: $treeStr")

    listCode[T, Int](q)(in)(
      onEmptyExpr = Expr(0),
      onTerms = xs => Expr(xs.size),
      failTree = failTree
    )

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

    listCode[T, String](q)(in)(
      onEmptyExpr = Expr("Nil"),
      onTerms = xs => Expr(s"List(${xs.map(transformTermToValue).mkString(", ")})"),
      failTree = failTree
    )


  def listCode[T, R](q: Quotes)(in: Expr[List[T]])(
    onEmptyExpr: => Expr[R],
    onTerms: List[q.reflect.Term] => Expr[R],
    failTree: => Nothing
  ): Expr[R] =
    import q.reflect.*

    def resolveIdent(ident: Ident): Option[Term] =
      ident.symbol.tree match
        case ValDef(_, _, Some(rhs))    => Some(rhs)
        case DefDef(_, _, _, Some(rhs)) => Some(rhs)
        case _                          => None

    def rec(tree: Term): Expr[R] =
      tree match
        case Inlined(_, _, i) =>
          rec(i)
        case Apply(TypeApply(Select(Ident("List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          onTerms(xs)
        case TypeApply(Select(Ident("List"), "empty"), _) =>
          onEmptyExpr
        case Ident("Nil") =>
          onEmptyExpr
        case Apply(TypeApply(Select(Select(Ident("immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          onTerms(xs)
        case TypeApply(Select(Select(Ident("immutable"), "List"), "empty"), _) =>
          onEmptyExpr
        case Select(Ident("immutable"), "Nil") =>
          onEmptyExpr
        case Apply(TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          onTerms(xs)
        case TypeApply(Select(Select(Select(Ident("collection"), "immutable"), "List"), "empty"), _) =>
          onEmptyExpr
        case Select(Select(Ident("collection"), "immutable"), "Nil") =>
          onEmptyExpr
        case Apply(TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "List"), "apply"), _), List(Typed(Repeated(xs, _), _))) =>
          onTerms(xs)
        case TypeApply(Select(Select(Select(Select(Ident("scala"), "collection"), "immutable"), "List"), "empty"), _) =>
          onEmptyExpr
        case Select(Select(Select(Ident("scala"), "collection"), "immutable"), "Nil") =>
          onEmptyExpr
        case ident @ Ident(_) =>
          resolveIdent(ident) match
            case None        =>
              failTree
            case Some(value) =>
              rec(value)
        case _ =>
          failTree

    rec(in.asTerm)