package de.sebbraun.hecate.auto

import scala.quoted.*

trait QuotesUtils[Q <: Quotes]:
  val q: Q
  private given Q = q
  import q.reflect.*

  object simplify extends TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term =
      tree match
        case Inlined(_, _, t) => transformTerm(t)(owner)
        case Block(Nil, t)    => transformTerm(t)(owner)
        case _                => super.transformTerm(tree)(owner)

    def apply(term: Term): Term = transformTerm(term)(term.symbol)

object QuotesUtils:
  extension (using Quotes)(r: quotes.reflect.reportModule)
    def error(message: String, pos: Option[quotes.reflect.Position]) = pos match
      case Some(pos0) => r.error(message, pos0)
      case None => r.error(message)

    def errorAndAbort(message: String, pos: Option[quotes.reflect.Position]) = pos match
      case Some(pos0) => r.errorAndAbort(message, pos0)
      case None => r.errorAndAbort(message)

    def info(message: String, pos: Option[quotes.reflect.Position]) = pos match
      case Some(pos0) => r.info(message, pos0)
      case None => r.info(message)

