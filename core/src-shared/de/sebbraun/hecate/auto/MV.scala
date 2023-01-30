package de.sebbraun.hecate.auto

import de.sebbraun.hecate.auto.MV.Inferred

import scala.quoted.*

sealed trait MV[+T, +M]:
  import MV.*

  def getOptionOrElse[U >: Option[T]](
      onConflict: Seq[Candidate[T, M]] => U,
      onFailed: List[Failure[M]] => U
  ): U =
    this match
      case Inferred(value, meta)           => Some(value)
      case Explicit(value, meta)           => Some(value)
      case Unknown                         => None
      case ConflictingInferred(candidates) => onConflict(candidates)
      case ConflictingExplicit(candidates) => onConflict(candidates)
      case Failed(failures)                => onFailed(failures)

  def getOrElse[U >: T](
      onUnknown: => U,
      onConflict: Seq[Candidate[T, M]] => U,
      onFailed: List[Failure[M]] => U
  ) =
    this match
      case Inferred(value, meta)           => value
      case Explicit(value, meta)           => value
      case ConflictingInferred(candidates) => onConflict(candidates)
      case ConflictingExplicit(candidates) => onConflict(candidates)
      case Unknown                         => onUnknown
      case Failed(failures)                => onFailed(failures)

  def getOrElse[U >: T](orElse: => U): U =
    this match
      case Inferred(v, _) => v
      case Explicit(v, _) => v
      case _              => orElse

  def conflicts: List[(T, Option[M])] =
    this match
      case ConflictingExplicit(c) => c.map(e => (e.value, e.meta))
      case ConflictingInferred(c) => c.map(i => (i.value, i.meta))
      case _                      => Nil

  def explicit[U >: T, N >: M](value: U, m: Option[N] = None): MV[U, N] =
    this match
      case Explicit(value0, _) if (value == value0) => this
      case e @ Explicit(_, _) =>
        ConflictingExplicit(List(Explicit(value, m), e))
      case ConflictingExplicit(l) =>
        ConflictingExplicit(Explicit(value, m) :: l)
      case _ => Explicit(value, m)

  def orInfer[U >: T, N >: M](value: U, m: Option[N] = None): MV[U, N] =
    this match
      case Unknown                                  => Inferred(value, m)
      case Inferred(value0, _) if (value == value0) => this
      case i @ Inferred(_, _) =>
        ConflictingInferred(List(Inferred(value, m), i))
      case ConflictingInferred(l) =>
        ConflictingInferred(Inferred(value, m) :: l)
      case _ => this

  def fail[N >: M](message: String, m: Option[N]): MV.Failed[N] =
    this match
      case Failed(l) => Failed(Failure(message, m) :: l)
      case _         => Failed(List(Failure(message, m)))

object MV:
  sealed trait Candidate[+T, +M] extends MV[T, M]:
    def value: T
    def meta: Option[M]

  object Present:
    def unapply[T, M](c: MV[T, M]): Option[(T, Option[M])] =
      c match
        case Inferred(value, meta) => Some((value, meta))
        case Explicit(value, meta) => Some((value, meta))
        case _                     => None

  sealed trait Conflict[+T, +M] extends MV[T, M]:
    def candidates: Seq[Candidate[T, M]]

  object Conflict:
    def unapplySeq[T, M](c: MV[T, M]): Option[Seq[Candidate[T, M]]] =
      c match
        case ConflictingInferred(candidates) => Some(candidates)
        case ConflictingExplicit(candidates) => Some(candidates)
        case _                               => None

  case object Unknown extends MV[Nothing, Nothing]

  case class Inferred[+T, +M](value: T, meta: Option[M]) extends Candidate[T, M]
  case class Explicit[+T, +M](value: T, meta: Option[M]) extends Candidate[T, M]

  case class ConflictingExplicit[+T, +M](candidates: List[Explicit[T, M]])
      extends Conflict[T, M]
  case class ConflictingInferred[+T, +M](candidates: List[Inferred[T, M]])
      extends Conflict[T, M]

  case class Failure[+M](message: String, meta: Option[M])
  case class Failed[+M](failures: List[Failure[M]]) extends MV[Nothing, M]

  private def reportConflicts[T](using q: Quotes)(
      msg: String,
      pos: Option[q.reflect.Position],
      sourceMsg: Option[T => String],
      cands: Iterable[Candidate[T, q.reflect.Position]]
  ) =
    import q.reflect.*
    import QuotesUtils.*

    sourceMsg.foreach { smsg =>
      cands.foreach { cand =>
        cand.meta.foreach { cpos => report.error(smsg(cand.value), cpos) }
      }
    }
    report.errorAndAbort(msg, pos)

  private def reportFailures(using q: Quotes)(
      fs: Seq[Failure[q.reflect.Position]]
  ): Nothing =
    import quotes.reflect.*
    import QuotesUtils.*

    val last = fs.length - 1
    fs.zipWithIndex.foreach { case (failure, index) =>
      if (index == last) report.errorAndAbort(failure.message, failure.meta)
      else report.error(failure.message, failure.meta)
    }
    throw IllegalArgumentException("Empty list of failures")

  extension [T](using q: Quotes)(mv: MV[T, q.reflect.Position])
    def getOrReportAndAbort(
      unknownMessage: => String,
      conflictMessage: => String,
      conflictSourceMessage: Option[T => String] = None,
      pos: Option[q.reflect.Position] = None
    ): T =
      import QuotesUtils.*
      import q.reflect.*
      mv.getOrElse(
        onUnknown = report.errorAndAbort(unknownMessage, pos),
        onConflict =
          reportConflicts(conflictMessage, pos, conflictSourceMessage, _),
        onFailed = reportFailures
      )

    def getOptionOrReportAndAbort(
      conflictMessage: => String,
      conflictSourceMessage: => Option[T => String] = None,
      pos: Option[quotes.reflect.Position],
    ): Option[T] =
      import QuotesUtils.*
      import q.reflect.*
      mv.getOptionOrElse(
        onConflict =
          reportConflicts(conflictMessage, pos, conflictSourceMessage, _),
        onFailed = reportFailures
      )
