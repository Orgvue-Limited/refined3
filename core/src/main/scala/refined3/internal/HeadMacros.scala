package refined3.internal

import refined3.{Expr => Expression, Proof}

import scala.quoted.*

object HeadMacros:
  import ForallMacros.{expressionFromExpr, transformTermToBool}

  inline def summonExpression[P: Type](expr: Expr[String])(failTree: => Nothing)(using q: Quotes): String =
    import q.reflect.*

    expr.asTerm.tpe.asType match
      case '[t] =>
        Expr.summon[Expression[t, P]] match
          case Some(value) => value.valueOrAbort.value
          case None        => failTree

  // List
  transparent inline def listExprStringHead[T, P](inline in: List[T]): String =
    ${ listHeadExprCode[T, P]('in) }

  def listHeadExprCode[T: Type, P: Type](in: Expr[List[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot produce Head Expr instance in compile time. Tree: $treeStr")

    val expr   = ListMacros.listStringCode(in)
    val expr0  = '{ "head(" + ${expr} + ")" }
    val string = summonExpression[P](expr0)(failTree)

    Expr(string)

  transparent inline def listHead[T, P](inline in: List[T]): Boolean =
    ${ listHeadCode[T, P]('in) }

  def listHeadCode[T: Type, P: Type](in: Expr[List[T]])(using q: Quotes): Expr[Boolean] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot apply Head predicate in compile time. Tree: $treeStr")

    ListMacros.listCode[T, Boolean](q)(in)(
      onEmptyExpr = Expr(true),
      onTerms = xs => Expr(xs.headOption.map(transformTermToBool[P](using q)(_, failTree)).getOrElse(false)),
      failTree = failTree
    )

  // Array
  transparent inline def arrayExprStringHead[T, P](inline in: Array[T]): String =
    ${ arrayHeadExprCode[T, P]('in) }

  def arrayHeadExprCode[T: Type, P: Type](in: Expr[Array[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot produce Head Expr instance in compile time. Tree: $treeStr")

    val expr   = ArrayMacros.arrayStringCode(in)
    val expr0  = '{ "head(" + ${expr} + ")" }
    val string = summonExpression[P](expr0)(failTree)

    Expr(string)

  transparent inline def arrayHead[T, P](inline in: Array[T]): Boolean =
    ${ arrayHeadCode[T, P]('in) }

  def arrayHeadCode[T: Type, P: Type](in: Expr[Array[T]])(using q: Quotes): Expr[Boolean] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot apply Head predicate in compile time. Tree: $treeStr")

    ArrayMacros.arrayCode[T, Boolean](q)(in)(
      onEmptyExpr = Expr(true),
      onTerms = xs => Expr(xs.headOption.map(transformTermToBool[P](using q)(_, failTree)).getOrElse(false)),
      failTree = failTree
    )

  // Vector
  transparent inline def vectorExprStringHead[T, P](inline in: Vector[T]): String =
    ${ vectorHeadExprCode[T, P]('in) }

  def vectorHeadExprCode[T: Type, P: Type](in: Expr[Vector[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot produce Head Expr instance in compile time. Tree: $treeStr")

    val expr   = VectorMacros.vectorStringCode(in)
    val expr0  = '{ "head(" + ${expr} + ")" }
    val string = summonExpression[P](expr0)(failTree)

    Expr(string)

  transparent inline def vectorHead[T, P](inline in: Vector[T]): Boolean =
    ${ vectorHeadCode[T, P]('in) }

  def vectorHeadCode[T: Type, P: Type](in: Expr[Vector[T]])(using q: Quotes): Expr[Boolean] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot apply Head predicate in compile time. Tree: $treeStr")

    VectorMacros.vectorCode[T, Boolean](q)(in)(
      onEmptyExpr = Expr(true),
      onTerms = xs => Expr(xs.headOption.map(transformTermToBool[P](using q)(_, failTree)).getOrElse(false)),
      failTree = failTree
    )