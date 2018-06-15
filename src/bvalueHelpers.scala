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
import Util._

/** Int-based factory/extractor for [[BBigInt]]s.
 *
 *  @group bfactory
 */
object BInt extends ToBValue[Int] {
  def apply(x: Int): BBigInt = BBigInt(x)
  def unapply(x: BBigInt): Option[Int] = if (x.value.isValidInt) Some(x.value.toInt) else None
}

/** Long-based factory/extractor for [[BBigInt]]s.
 *
 *  @group bfactory
 */
object BLong extends ToBValue[Long] {
  def apply(x: Long): BBigInt = BBigInt(x)
  def unapply(x: BBigInt): Option[Long] = if (x.value.isValidLong) Some(x.value.toLong) else None
}

/** Factory/extractor for UTF-8-encoded [[BBinary]] strings.
 *
 *  @see #BAnyString
 *
 *  @group bfactory
 */
object BString extends ToBValue[String] with ToBBinary[String] {
  def apply(x: String): BBinary = BBinary(encodeUTF8(x))
  def unapply(x: BBinary): Option[String] = decodeUTF8(x.toArray).toOption
}

/** Factory/extractor for [[BBinary]] strings of unknown encoding.
 *
 *  This is a variant of [[#BString]] which will fallback to Windows-1252 (itself a superset of
 *  Latin-1 and US-ASCII) if the text is not valid UTF-8. Such fallback is necessary when parsing
 *  dirty input such as filenames which are just a bag-of-bytes on many platforms and not always
 *  correctly encoded when generating torrent files.
 *
 *  @see #BString
 *
 *  @group bfactory
 */
object BAnyString extends ToBValue[String] with ToBBinary[String] {

  def apply(x: String): BBinary = BString(x)

  def unapply(x: BBinary): Option[String] = {
    val bs = x.toArray
    decodeUTF8(bs) orElse decodeCP1252(bs)
  }.toOption

}

/** Symbol-based factory/extractor for [[BBinary]]s.
 *
 *  @group bfactory
 */
object BSymbol extends ToBValue[Symbol] with ToBBinary[Symbol] {
  def apply(x: Symbol): BBinary = BString(x.name)
  def unapply(x: BBinary): Option[Symbol] = BString.unapply(x) map Symbol.apply
}
