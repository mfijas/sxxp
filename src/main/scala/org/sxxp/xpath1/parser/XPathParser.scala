/*
 * Copyright 2014 Michał Fijas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.sxxp.xpath1.parser

import scala.util.parsing.combinator.{PackratParsers, JavaTokenParsers}
import org.sxxp.xpath1.parser.expression._
import org.sxxp.xpath1.parser.step._
import org.sxxp.xpath1.parser.nodetest._
import org.sxxp.xpath1.parser.path._
import org.sxxp.xpath1.utils.Logging
import org.sxxp.xpath1.parser.axis._
import org.sxxp.xpath1.parser.expression.LtEExpression
import org.sxxp.xpath1.parser.expression.UnionExpression
import org.sxxp.xpath1.parser.expression.EqExpression
import org.sxxp.xpath1.parser.step.NodeStep
import org.sxxp.xpath1.parser.expression.LocationPathExpression
import org.sxxp.xpath1.parser.expression.LtExpression
import org.sxxp.xpath1.parser.expression.NumberExpression
import scala.Some
import org.sxxp.xpath1.parser.expression.MinusExpression
import org.sxxp.xpath1.parser.expression.FunctionCallExpression
import org.sxxp.xpath1.parser.expression.OrExpression
import org.sxxp.xpath1.parser.expression.FilterExpression
import org.sxxp.xpath1.parser.expression.GtEExpression
import org.sxxp.xpath1.parser.expression.DivExpression
import org.sxxp.xpath1.parser.expression.PathExpression
import org.sxxp.xpath1.parser.expression.AbbreviatedPathExpression
import org.sxxp.xpath1.parser.expression.SumExpression
import org.sxxp.xpath1.parser.path.RelativeLocationPath
import org.sxxp.xpath1.parser.expression.NeqExpression
import org.sxxp.xpath1.parser.expression.ModExpression
import org.sxxp.xpath1.parser.expression.MultiplyExpression
import org.sxxp.xpath1.parser.expression.LiteralExpression
import org.sxxp.xpath1.parser.nodetest.NameNodeTest
import org.sxxp.xpath1.parser.nodetest.NodeTypeTest
import org.sxxp.xpath1.parser.expression.SubtractExpression
import org.sxxp.xpath1.parser.step.AbbreviatedNodeStep
import org.sxxp.xpath1.parser.expression.AndExpression
import org.sxxp.xpath1.parser.expression.GtExpression


class XPathParser extends JavaTokenParsers with PackratParsers with Logging {

  def println(s: String, args: Object*) {
    logger.debug(s, args: _*)
  }

  override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser {
    in =>
      println("trying {} at {}", name, in)
      val r = p(in)
      println("{} --> {}", name, r)
      r
  }

  def parsePathExpression(input: String) = parseAll(pathExpr, input).get

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
    log(step)("step") ^^ (s => RelativeLocationPath(List(s))) |||
      log(relativeLocationPath ~ "/" ~ step)("relativeLocationPath ~ \"/\" ~ step") ^^ {
        case path ~ _ ~ step => path :+ step
      } |||
      log(abbreviatedRelativeLocationPath)("abbreviatedRelativeLocationPath")

  def step: Parser[Step] =
    axisSpecifier ~ nodeTest ~ rep(predicate) ^^ {
      case axisSpecifier ~ nodeTest ~ predicates => NodeStep(axisSpecifier, nodeTest, predicates)
    } |||
      abbreviatedStep

  def axisSpecifier: Parser[Axis] =
    axisName <~ "::" ^^ {
      axis => axis
    } |
      abbreviatedAxisSpecifier ^^ {
        axis => axis
      }

  def axisName: Parser[Axis] =
    "ancestor" ^^^ AncestorAxis |
      "ancestor-or-self" ^^^ AncestorOrSelfAxis |
      "attribute" ^^^ AttributeAxis |
      "child" ^^^ ChildAxis |
      "descendant" ^^^ DescendantAxis |
      "descendant-or-self" ^^^ DescendantOrSelfAxis |
      "following" ^^^ FollowingAxis |
      "following-sibling" ^^^ FollowingSiblingAxis |
      "namespace" ^^^ NamespaceAxis |
      "parent" ^^^ ParentAxis |
      "preceding" ^^^ PrecedingAxis |
      "preceding-sibling" ^^^ PrecedingSiblingAxis |
      "self" ^^^ SelfAxis

  /* | nodeType <~ "()" | "processing-instruction("~literal~")" */
  def nodeTest: Parser[NodeTest] =
    nameTest ^^ (qname => NameNodeTest(qname)) |
      nodeType <~ "()" ^^ (t => NodeTypeTest(t))

  def predicate: Parser[Predicate] = "[" ~> predicateExpr <~ "]" ^^ (e => Predicate(e))

  def predicateExpr: Parser[Expression] = expr

  lazy val abbreviatedAbsoluteLocationPath: PackratParser[LocationPath] =
    log("//" ~> relativeLocationPath)("\"//\" ~> relativeLocationPath") ^^ (AbbreviatedAbsoluteLocationPath(_))

  lazy val abbreviatedRelativeLocationPath: PackratParser[LocationPath] =
    log(relativeLocationPath ~ "//" ~ step)("relativeLocationPath ~ \"//\" ~ step") ^^ {
      case path ~ _ ~ step => path :+ AbbreviatedNodeStep(step)
    }

  def abbreviatedStep: Parser[Step] =
    "." ^^^ CurNodeStep |
      ".." ^^^ ParentNodeStep

  def abbreviatedAxisSpecifier: Parser[Axis] = opt("@") ^^ {
    at => if (at.isDefined) AttributeAxis else ChildAxis
  }

  def expr: Parser[Expression] = orExpr

  def stripQuotes(s: String) = s.substring(1, s.length - 1)

  def primaryExpr: Parser[Expression] =
  /*variableReference |*/
    log("(" ~> expr <~ ")")("\"(\" ~> expr <~ \")\"") |||
      log(literal)("literal") ^^ (s => LiteralExpression(stripQuotes(s))) |||
      log(number)("number") ^^ (n => NumberExpression(n)) |||
      log(functionCall)("functionCall")

  def functionCall: Parser[FunctionCallExpression] =
    log(functionName ~ "(" ~ repsep(argument, ",") ~ ")")("functionName ~ \"(\" ~ repsep(argument, \",\") ~ \")\"") ^^ {
      case funName ~ "(" ~ args ~ ")" => FunctionCallExpression(funName, args)
    }

  def functionName: Parser[QName] =
    qname

  def argument: Parser[Expression] =
    expr

  lazy val unionExpr: PackratParser[Expression] =
    pathExpr |
      unionExpr ~ "|" ~ pathExpr ^^ {
        case e1 ~ _ ~ e2 => UnionExpression(e1, e2)
      }

  //  lazy val pathExpr: PackratParser[Expression] =
  def pathExpr: Parser[Expression] =
    log(locationPath)("locationPath") ^^ (path => LocationPathExpression(path)) |||
      log(filterExpr)("filterExpr") |||
      log(filterExpr ~ "/" ~ relativeLocationPath)("filterExpr ~ \"/\" ~ relativeLocationPath") ^^ {
        case prefix ~ _ ~ tail => PathExpression(prefix, tail)
      } |||
      log(filterExpr ~ "//" ~ relativeLocationPath)("filterExpr ~ \"//\" ~ relativeLocationPath") ^^ {
        case prefix ~ _ ~ tail => AbbreviatedPathExpression(prefix, tail)
      }

  lazy val filterExpr: PackratParser[Expression] =
    primaryExpr |||
      filterExpr ~ predicate ^^ {
        case e ~ pred => FilterExpression(e, pred)
      }


  lazy val orExpr: PackratParser[Expression] =
    andExpr |
      orExpr ~ "or" ~ andExpr ^^ {
        case e1 ~ _ ~ e2 => OrExpression(e1, e2)
      }

  lazy val andExpr: PackratParser[Expression] =
    equalityExpr |
      andExpr ~ "and" ~ equalityExpr ^^ {
        case e1 ~ _ ~ e2 => AndExpression(e1, e2)
      }

  lazy val equalityExpr: PackratParser[Expression] =
    relationalExpr |||
      equalityExpr ~ "=" ~ relationalExpr ^^ {
        case e1 ~ _ ~ e2 => EqExpression(e1, e2)
      } |
      equalityExpr ~ "!=" ~ relationalExpr ^^ {
        case e1 ~ _ ~ e2 => NeqExpression(e1, e2)
      }

  lazy val relationalExpr: PackratParser[Expression] =
    additiveExpr |
      relationalExpr ~ "<" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => LtExpression(e1, e2)
      } |
      relationalExpr ~ ">" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => GtExpression(e1, e2)
      } |
      relationalExpr ~ "<=" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => LtEExpression(e1, e2)
      } |
      relationalExpr ~ ">=" ~ additiveExpr ^^ {
        case e1 ~ _ ~ e2 => GtEExpression(e1, e2)
      }

  lazy val additiveExpr: PackratParser[Expression] =
    multiplicativeExpr |
      additiveExpr ~ "+" ~ multiplicativeExpr ^^ {
        case e1 ~ _ ~ e2 => SumExpression(e1, e2)
      } |
      additiveExpr ~ "-" ~ multiplicativeExpr ^^ {
        case e1 ~ _ ~ e2 => SubtractExpression(e1, e2)
      }

  lazy val multiplicativeExpr: PackratParser[Expression] =
    unaryExpr |
      multiplicativeExpr ~ "*" ~ unaryExpr ^^ {
        case e1 ~ _ ~ e2 => MultiplyExpression(e1, e2)
      } |
      multiplicativeExpr ~ "div" ~ unaryExpr ^^ {
        case e1 ~ _ ~ e2 => DivExpression(e1, e2)
      } |
      multiplicativeExpr ~ "mod" ~ unaryExpr ^^ {
        case e1 ~ _ ~ e2 => ModExpression(e1, e2)
      }

  lazy val unaryExpr: PackratParser[Expression] =
    unionExpr |
      "-" ~> unaryExpr ^^ (e => MinusExpression(e))

  // exprToken

  def literal: Parser[String] = """"[^"]*"""".r | "'[^']*'".r

  def number: Parser[Double] = decimalNumber ^^ (_.toDouble)

  // digits
  // operator
  // operatorName
  // multiplyOperator
  // functionName
  // variableReference

  // TODO fix ns-handling logic in ncname:*
  def nameTest: Parser[QName] =
    "*" ^^^ QName("*") |
      ncname <~ ":*" ^^ (QName("", _, "*")) |
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
  // TODO add namespace resolution logic
  def prefixedName: Parser[QName] = (prefix ~ ":" ~ localPart) ^^ {
    case prefix ~ ":" ~ localPart => QName("", prefix, localPart)
  }

  // http://www.w3.org/TR/REC-xml-names/#NT-Prefix
  def prefix: Parser[String] = ncname

  // http://www.w3.org/TR/REC-xml-names/#NT-QName
  def qname: Parser[QName] = prefixedName | unprefixedName


}