package refined3.internal

import scala.quoted.*

object ArrayMacros:
  transparent inline def arraySize[T](inline in: Array[T]): Int =
    ${ arraySizeCode('in) }

  def arraySizeCode[T: Type](in: Expr[Array[T]])(using q: Quotes): Expr[Int] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot determine size of array at compile time. Tree: $treeStr")

    arrayCode[T, Int](q)(in)(
      onEmptyExpr = Expr(0),
      onTerms = xs => Expr(xs.size),
      failTree = failTree
    )

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

    arrayCode[T, String](q)(in)(
      onEmptyExpr = Expr("Array()"),
      onTerms = xs => Expr(s"Array(${xs.map(transformTermToValue).mkString(", ")})"),
      failTree = failTree
    )

  def arrayCode[T, R](q: Quotes)(in: Expr[Array[T]])(
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
        case Apply(TypeApply(Select(Ident("Array"), "empty"), _), _) =>
          onEmptyExpr
        case Apply(Select(Ident("Array"), "apply"), List(lit @ Literal(_), Typed(Repeated(xs, _), _))) =>
          onTerms(lit :: xs)
        case Apply(Select(Ident("Array"), "apply"), List(Typed(Repeated(xs, _), _))) =>
          onTerms(xs)
        case Apply(Apply(TypeApply(Select(Ident("Array"), "apply"), _), List(Typed(Repeated(xs, _), _))), _) =>
          onTerms(xs)
        case Apply(TypeApply(Select(Select(Ident("scala"), "Array"), "empty"), _), _) =>
          onEmptyExpr
        case Apply(Select(Select(Ident("scala"), "Array"), "apply"), List(lit @ Literal(_), Typed(Repeated(xs, _), _))) =>
          onTerms(lit :: xs)
        case Apply(Select(Select(Ident("scala"), "Array"), "apply"), List(Typed(Repeated(xs, _), _))) =>
          onTerms(xs)
        case Apply(Apply(TypeApply(Select(Select(Ident("scala"), "Array"), "apply"), _), List(Typed(Repeated(xs, _), _))), _) =>
          onTerms(xs)
        case ident @ Ident(_) =>
          resolveIdent(ident) match
            case None        =>
              failTree
            case Some(value) =>
              rec(value)
        case _ =>
          failTree

    rec(in.asTerm)