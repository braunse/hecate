package de.sebbraun.hecate

class RecordSchema[T, FS <: RecordFieldSchema[T]](
    val fields: FS,
    val constructor: fields.Tail => T
)

object RecordSchema:
  class PartiallyApplied[T]:
    def of[FS <: RecordFieldSchema[T]](fields: FS, cons: fields.Tail => T) =
      new RecordSchema(fields, cons)
  def apply[T]: PartiallyApplied[T] = PartiallyApplied[T]()

sealed trait RecordFieldSchema[T]:
  type Tail <: Tuple

case class FieldSchema[T, F, N <: RecordFieldSchema[T]](
    val name: String,
    val getter: T => F,
    val next: N
) extends RecordFieldSchema[T]:
  type FieldType = F
  type Tail = F *: next.Tail

case class EmptySchema[T]() extends RecordFieldSchema[T]:
  type Tail = EmptyTuple
