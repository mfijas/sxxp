package org.sxxp.xpath1.parser

sealed trait Expr

case class OrExpr(left: Expr, right: Expr) extends Expr

case class AndExpr(left: Expr, right: Expr) extends Expr

case class EqExpr(left: Expr, right: Expr) extends Expr

case class NeqExpr(left: Expr, right: Expr) extends Expr

case class LtExpr(left: Expr, right: Expr) extends Expr

case class GtExpr(left: Expr, right: Expr) extends Expr

case class LtEExpr(left: Expr, right: Expr) extends Expr

case class GtEExpr(left: Expr, right: Expr) extends Expr

case class SumExpr(left: Expr, right: Expr) extends Expr

case class SubtractExpr(left: Expr, right: Expr) extends Expr

case class MultiplyExpr(left: Expr, right: Expr) extends Expr

case class DivExpr(left: Expr, right: Expr) extends Expr

case class ModExpr(left: Expr, right: Expr) extends Expr

case class MinusExpr(exp: Expr) extends Expr

case class UnionExpr(left: Expr, right: Expr) extends Expr

case class LiteralExpr(s: String) extends Expr

case class NumberExpr(v: Double) extends Expr

case class FilterExpr(expr: Expr, pred: Predicate) extends Expr

case class PathExpr(expr: Expr, path: LocationPath) extends Expr

case class AbbreviatedPathExpr(expr: Expr, path: LocationPath) extends Expr

case class LocationPathExpr(path: LocationPath) extends Expr