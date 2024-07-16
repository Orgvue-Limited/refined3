package refined3.internal

import refined3.{Expr => Expression, Proof}

import scala.quoted.*

object ForallMacros:
  given expressionFromExpr[T: Type, P: Type]: FromExpr[Expression[T, P]] =
    new FromExpr[Expression[T, P]]:
      def unapply(expr: Expr[Expression[T, P]])(using Quotes): Option[Expression[T, P]] =
        expr match
          case '{ Expression[T, P]($value) } =>
            Some(Expression[T, P](value.valueOrAbort))
          case _                             =>
            None

  given proofFromExpr[T: Type, P: Type]: FromExpr[Proof[T, P]] =
    new FromExpr[Proof[T, P]]:
      def unapply(expr: Expr[Proof[T, P]])(using Quotes): Option[Proof[T, P]] =
        expr match
          case '{ ($x: Proof.Successful[T, P]) } =>
            Some(Proof.success[T, P])
          case '{ ($x: Proof.Failure[T, P]) } =>
            Some(Proof.failure[T, P])
          case _                             =>
            None

  def transformTermToExpr[P: Type](using q: Quotes)(tree: q.reflect.Term, failTree: => Nothing): String =
    import q.reflect.*

    tree match
      case Literal(constant) =>
        tree.tpe.asType match
          case '[typ] =>
            Expr.summon[Expression[typ, P]] match
              case Some(value) => value.valueOrAbort.value
              case None        => failTree

      case _ =>
        failTree

  def transformTermToBool[P: Type](using q: Quotes)(tree: q.reflect.Term, failTree: => Nothing): Boolean =
    import q.reflect.*

    tree match
      case Literal(constant) =>
        tree.tpe.asType match
          case '[typ] =>
            Expr.summon[Proof[typ, P]] match
              case Some(value) =>
                value.valueOrAbort match
                  case _: Proof.Successful[?, ?] => true
                  case _: Proof.Failure[?, ?]    => false
              case None        =>
                failTree

      case _ =>
        failTree

  // List
  transparent inline def listExprStringForAll[T, P](inline in: List[T]): String =
    ${ listForAllExprCode[T, P]('in) }

  def listForAllExprCode[T: Type, P: Type](in: Expr[List[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot produce forall Expr instance in compile time. Tree: $treeStr")

    ListMacros.listCode[T, String](q)(in)(
      onEmptyExpr = Expr(""),
      onTerms = xs => Expr(xs.map(transformTermToExpr[P](using q)(_, failTree)).mkString(" && ")),
      failTree = failTree
    )

  transparent inline def listForAll[T, P](inline in: List[T]): Boolean =
    ${ listForAllCode[T, P]('in) }

  def listForAllCode[T: Type, P: Type](in: Expr[List[T]])(using q: Quotes): Expr[Boolean] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot apply forall predicate in compile time. Tree: $treeStr")

    ListMacros.listCode[T, Boolean](q)(in)(
      onEmptyExpr = Expr(true),
      onTerms = xs => Expr(xs.forall(transformTermToBool[P](using q)(_, failTree))),
      failTree = failTree
    )

  // Array
  transparent inline def arrayExprStringForAll[T, P](inline in: Array[T]): String =
    ${ arrayForAllExprCode[T, P]('in) }

  def arrayForAllExprCode[T: Type, P: Type](in: Expr[Array[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot produce forall Expr instance in compile time. Tree: $treeStr")

    ArrayMacros.arrayCode[T, String](q)(in)(
      onEmptyExpr = Expr(""),
      onTerms = xs => Expr(xs.map(transformTermToExpr[P](using q)(_, failTree)).mkString(" && ")),
      failTree = failTree
    )

  transparent inline def arrayForAll[T, P](inline in: Array[T]): Boolean =
    ${ arrayForAllCode[T, P]('in) }

  def arrayForAllCode[T: Type, P: Type](in: Expr[Array[T]])(using q: Quotes): Expr[Boolean] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot apply forall predicate in compile time. Tree: $treeStr")

    ArrayMacros.arrayCode[T, Boolean](q)(in)(
      onEmptyExpr = Expr(true),
      onTerms = xs => Expr(xs.forall(transformTermToBool[P](using q)(_, failTree))),
      failTree = failTree
    )

  // Vector
  transparent inline def vectorExprStringForAll[T, P](inline in: Vector[T]): String =
    ${ vectorForAllExprCode[T, P]('in) }

  def vectorForAllExprCode[T: Type, P: Type](in: Expr[Vector[T]])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot produce forall Expr instance in compile time. Tree: $treeStr")

    VectorMacros.vectorCode[T, String](q)(in)(
      onEmptyExpr = Expr(""),
      onTerms = xs => Expr(xs.map(transformTermToExpr[P](using q)(_, failTree)).mkString(" && ")),
      failTree = failTree
    )

  transparent inline def vectorForAll[T, P](inline in: Vector[T]): Boolean =
    ${ vectorForAllCode[T, P]('in) }

  def vectorForAllCode[T: Type, P: Type](in: Expr[Vector[T]])(using q: Quotes): Expr[Boolean] =
    import quotes.reflect.*

    def failTree: Nothing =
      val treeStr = in.asTerm.show(using Printer.TreeStructure)
      q.reflect.report.errorAndAbort(s"Cannot apply forall predicate in compile time. Tree: $treeStr")

    VectorMacros.vectorCode[T, Boolean](q)(in)(
      onEmptyExpr = Expr(true),
      onTerms = xs => Expr(xs.forall(transformTermToBool[P](using q)(_, failTree))),
      failTree = failTree
    )