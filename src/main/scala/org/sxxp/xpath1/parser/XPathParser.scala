package org.sxxp.xpath1.parser

import scala.util.parsing.combinator.{PackratParsers, JavaTokenParsers}


class XPathParser extends JavaTokenParsers with PackratParsers {

  def locationPath: Parser[LocationPath] =
    log(relativeLocationPath)("relativeLocationPath") |||
      log(absoluteLocationPath)("absoluteLocationPath")

  def absoluteLocationPath: Parser[LocationPath] =
    log("/" ~> opt(relativeLocationPath))("\"/\" ~> opt(relativeLocationPath)") ^^ {
      case Some(path) => AbsoluteLocationPath(path)
      case None => RootLocationPath
    } |||
      log(abbreviatedAbsoluteLocationPath)("abbreviatedAbsoluteLocationPath")

  lazy val relativeLocationPath: PackratParser[LocationPath] =
    log(step)("step") ^^ (RelativeLocationPath(_)) |||
      log(relativeLocationPath ~ "/" ~ step)("relativeLocationPath ~ \"/\" ~ step") ^^ {
        case path ~ _ ~ step => path :+ step
      } |||
      log(abbreviatedRelativeLocationPath)("abbreviatedRelativeLocationPath")

  def step: Parser[Step] =
  /*axisSpecifier ~ */ nodeTest ~ rep(predicate) ^^ {
    case /*axisSpecifier ~ */ nodeTest ~ predicates => NodeStep(nodeTest, predicates)
  } |||
    abbreviatedStep

  /*
      def axisSpecifier: Parser[AxisSpecifier] = (axisName ~ "::" | abbreviatedAxisSpecifier) ^^ {
        case (axisName: String) ~ "::" => AxisSpecifier(axisName)
        case abbreviatedAxisSpecifier: String => AxisSpecifier(abbreviatedAxisSpecifier)
      }

      def axisName: Parser[String] = "ancestor" | "ancestor-or-self" | "attribute" | "child" | "descendant" |
        "descendant-or-self" | "following" | "following-sibling" | "namespace" | "parent" | "preceding" |
        "preceding-sibling" | "self"
  */

  /* | nodeType <~ "()" | "processing-instruction("~literal~")" */
  def nodeTest: Parser[NodeTest] =
    nameTest ^^ (qname => NameNodeTest(qname)) |
      nodeType <~ "()" ^^ (t => NodeTypeTest(t))

  def predicate: Parser[Predicate] = "[" ~> predicateExpr <~ "]" ^^ (e => Predicate(e))

  def predicateExpr: Parser[Expr] = expr

  lazy val abbreviatedAbsoluteLocationPath: PackratParser[LocationPath] =
    log("//" ~> relativeLocationPath)("\"//\" ~> relativeLocationPath") ^^ (AbbreviatedAbsoluteLocationPath(_))

  lazy val abbreviatedRelativeLocationPath: PackratParser[LocationPath] =
    log(relativeLocationPath ~ "//" ~ step)("relativeLocationPath ~ \"//\" ~ step") ^^ {
      case path ~ _ ~ step => path :+ AbbreviatedNodeStep(step)
    }

  def abbreviatedStep: Parser[Step] =
    "." ^^^ CurNodeStep |
      ".." ^^^ ParentNodeStep

  /*
    def abbreviatedAxisSpecifier: Parser[String] = opt("@") ^^ (_.getOrElse(""))
  */

  def expr: Parser[Expr] = orExpr

  def stripQuotes(s: String) = s.substring(1, s.length - 1)

  def primaryExpr: Parser[Expr] =
  /*variableReference |*/
    "(" ~> expr <~ ")" |
      literal ^^ (s => LiteralExpr(stripQuotes(s))) |
      number ^^ (n => NumberExpr(n))

  /*|
     functionCall*/


  // functionCall
  //argument

  lazy val unionExpr: PackratParser[Expr] =
    pathExpr |
      unionExpr ~ "|" ~ pathExpr ^^ {
        case e1 ~ _ ~ e2 => UnionExpr(e1, e2)
      }

  //  lazy val pathExpr: PackratParser[Expr] =
  def pathExpr: Parser[Expr] =
    log(locationPath)("locationPath") ^^ (path => LocationPathExpr(path)) |
      log(filterExpr)("filterExpr") |
      log(filterExpr ~ "/" ~ relativeLocationPath)("filterExpr ~ \"/\" ~ relativeLocationPath") ^^ {
        case prefix ~ _ ~ tail => PathExpr(prefix, tail)
      } |
      log(filterExpr ~ "//" ~ relativeLocationPath)("filterExpr ~ \"//\" ~ relativeLocationPath") ^^ {
        case prefix ~ _ ~ tail => AbbreviatedPathExpr(prefix, tail)
      }

  lazy val filterExpr: PackratParser[Expr] =
    primaryExpr |
      filterExpr ~ predicate ^^ {
        case e ~ pred => FilterExpr(e, pred)
      }


  lazy val orExpr: PackratParser[Expr] =
    andExpr |
      orExpr ~ "or" ~ andExpr ^^ {
        case e1 ~ _ ~ e2 => OrExpr(e1, e2)
      }

  lazy val andExpr: PackratParser[Expr] =
    equalityExpr |
      andExpr ~ "and" ~ equalityExpr ^^ {
        case e1 ~ _ ~ e2 => AndExpr(e1, e2)
      }

  lazy val equalityExpr: PackratParser[Expr] =
    relationalExpr |||
      equalityExpr ~ "=" ~ relationalExpr ^^ {
        case e1 ~ _ ~ e2 => EqExpr(e1, e2)
      } |
      equalityExpr ~ "!=" ~ relationalExpr ^^ {
        case e1 ~ _ ~ e2 => NeqExpr(e1, e2)
      }

  lazy val relationalExpr: PackratParser[Expr] =
    additiveExpr |
      relationalExpr ~ "<" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => LtExpr(e1, e2)
      } |
      relationalExpr ~ ">" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => GtExpr(e1, e2)
      } |
      relationalExpr ~ "<=" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => LtEExpr(e1, e2)
      } |
      relationalExpr ~ ">=" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => GtEExpr(e1, e2)
      }

  lazy val additiveExpr: PackratParser[Expr] =
    multiplicativeExpr |
      additiveExpr ~ "+" ~ multiplicativeExpr ^^ {
        case e1 ~ _ ~ e2 => SumExpr(e1, e2)
      } |
      additiveExpr ~ "-" ~ multiplicativeExpr ^^ {
        case e1 ~ _ ~ e2 => SubtractExpr(e1, e2)
      }

  lazy val multiplicativeExpr: PackratParser[Expr] =
    unaryExpr |
      multiplicativeExpr ~ "*" ~ unaryExpr ^^ {
        case e1 ~ _ ~ e2 => MultiplyExpr(e1, e2)
      } |
      multiplicativeExpr ~ "div" ~ unaryExpr ^^ {
        case e1 ~ _ ~ e2 => DivExpr(e1, e2)
      } |
      multiplicativeExpr ~ "mod" ~ unaryExpr ^^ {
        case e1 ~ _ ~ e2 => ModExpr(e1, e2)
      }

  lazy val unaryExpr: PackratParser[Expr] =
    unionExpr |
      "-" ~> unaryExpr ^^ (e => MinusExpr(e))

  // exprToken

  def literal: Parser[String] = """"[^"]*"""".r | "'[^']*'".r

  def number: Parser[Double] = decimalNumber ^^ (_.toDouble)

  // digits
  // operator
  // operatorName
  // multiplyOperator
  // functionName
  // variableReference

  def nameTest: Parser[QName] =
    "*" ^^ (_ => QName("*")) |
      ncname <~ ":*" ^^ (QName(_, "*")) |
      qname

  def nodeType: Parser[NodeType] =
    "comment" ^^^ CommentNodeType |
      "text" ^^^ TextNodeType |
      "processing-instruction" ^^^ ProcessingInstructionNodeType |
      "node" ^^^ NodeNodeType

  // exprWhitespace

  // http://www.w3.org/TR/REC-xml/#NT-Name
  // TODO change to match the spec
  def name: Parser[String] = ident

  // http://www.w3.org/TR/REC-xml-names/#NT-NCName
  def ncname: Parser[String] = name

  // http://www.w3.org/TR/REC-xml-names/#NT-LocalPart
  def localPart: Parser[String] = ncname

  // http://www.w3.org/TR/REC-xml-names/#NT-UnprefixedName
  def unprefixedName: Parser[QName] = localPart ^^ {
    case localPart => QName(localPart)
  }

  // http://www.w3.org/TR/REC-xml-names/#NT-PrefixedName
  def prefixedName: Parser[QName] = (prefix ~ ":" ~ localPart) ^^ {
    case prefix ~ ":" ~ localPart => QName(prefix, localPart)
  }

  // http://www.w3.org/TR/REC-xml-names/#NT-Prefix
  def prefix: Parser[String] = ncname

  // http://www.w3.org/TR/REC-xml-names/#NT-QName
  def qname: Parser[QName] = prefixedName | unprefixedName


}