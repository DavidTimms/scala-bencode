// Copyright (c) 2018 Peter Corlett. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this list of conditions
//    and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice, this list of
//    conditions and the following disclaimer in the documentation and/or other materials provided
//    with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
// WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package net.mooli.bencode

import scala.language.implicitConversions

import util.{Try, Failure, Success}

private[bencode] object Util {

  import java.nio.charset.StandardCharsets.UTF_8

  // Windows often emits CP1252 and calls it Latin-1. It's a superset, so we can safely decode
  // Latin-1 using CP1252 and have it still work.
  lazy val CP1252 = java.nio.charset.Charset.forName("cp1252")
  lazy val decodeFailure = Failure(new java.nio.charset.CharacterCodingException)

  def encodeUTF8(x: String): Array[Byte] = x getBytes UTF_8

  def decodeUTF8(x: Array[Byte]): Try[String] = new String(x, UTF_8) match {
    case s if s contains '\uFFFD' => decodeFailure
    case s => Success(s)
  }

  def decodeCP1252(x: Array[Byte]): Try[String] = new String(x, CP1252) match {
    case s if s contains '\uFFFD' => decodeFailure
    case s => Success(s)
  }

  def escaped(c: Byte): String = c & 255 match {
    case '\n' => "\\n"
    case '"' => "\\\""
    case c if c >= 32 & c <= 126 => "%c" format c
    case c => "\\%03o" format c
  }

}

/** Exception thrown on bencode parse errors. */
class DecodeException(s: String) extends java.lang.Exception(s)

/** A Builder-like type used by [[BValue.write]] for serialising bencoded data.
 *
 * @group generating
 */
// This class exists for a couple of reasons. The first is that it has a simpler API than a regular
// Builder, and so is easier to adapt to. The second is that it enables implicit conversions to it,
// including from a regular Builder. This means that BValue.write can directly accept a Builder or
// another adapted type.
trait BBuilder {
  /** adds the byte to the builder */
  def +=(b: Byte): this.type // scalastyle:ignore method.name

  /** appends the bytes to the builder */
  def ++=(bs: Array[Byte]): this.type = {
    bs foreach this.+=;
    this
  } // scalastyle:ignore method.name
}

/** Factory for [[BBuilder]]s.
 *
 * @group generating
 */
object BBuilder {
  /** Implicit any-to-BBuilder factory via ToBBuilder typeclass. */
  implicit def apply[T: ToBBuilder](value: T): BBuilder = implicitly[ToBBuilder[T]] apply value
}

/** An Iterator-like type used by [[BValue#read]] for deserialising bencoded data.
 *
 * @group parsing
 */
// Similar to BBuilder, this class mainly exists to provide a potentially more efficient nextSeq
// implementation, and for easier implicit conversions for BValue#read.
trait BIterator {
  /** Reads the next byte from the sequence. */
  def next(): Byte

  /** Reads the next `length` bytes from the sequence.
   *
   * The default implementation assembles the array by calling [[.next]] `length` times. You are
   * encouraged to replace this with a more efficient implementation if your underlying data source
   * supports it.
   */
  def nextSeq(length: Int): Array[Byte] = Array.fill(length) {
    this.next()
  }

}

/** Factory for [[BIterator]]s.
 *
 * @group parsing
 */
object BIterator {
  /** Implicit any-to-BIterator factory via ToBIterator typeclass. */
  implicit def apply[T: ToBIterator](value: T): BIterator = implicitly[ToBIterator[T]] apply value
}
