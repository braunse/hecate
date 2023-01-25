package de.sebbraun.hecate

import io.circe.*

class Codec[T](
    val encoder: Encoder[T],
    val decoder: Decoder[T]
)

given codec2encoder[T](using codec: Codec[T]): Encoder[T] = codec.encoder
given codec2decoder[T](using codec: Codec[T]): Decoder[T] = codec.decoder
