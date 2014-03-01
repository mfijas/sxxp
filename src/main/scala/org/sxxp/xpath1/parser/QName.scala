package org.sxxp.xpath1.parser

case class QName(ns: String, prefix: String, localPart: String) {
  def getFullName = if (ns.nonEmpty) s"{$ns}$localPart" else localPart
}

object QName {
  def apply(localPart: String): QName = QName("", "", localPart)
}
