package org.sxxp.xpath1.parser.function

import org.sxxp.xpath1.parser.types.XObject

trait XPathFunction[A] extends (A => XObject)
