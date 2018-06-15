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

import annotation.implicitNotFound
import collection.generic.Growable

/** Implicit conversion to [[BBinary]].
 *
 *  @group bfactory
 */
@implicitNotFound("No member of type class ToBBinary in scope for ${T}")
trait ToBBinary[T] extends (T => BBinary)

/** Precanned implicit conversions to [[BBinary]].
 *
 *  @group bfactory
 */
object ToBBinary {
  implicit def fromString: ToBBinary[String] = BString
  implicit def fromSymbol: ToBBinary[Symbol] = BSymbol
}

/** Implicit conversion to [[BValue]].
 *
 *  @group bfactory
 */
@implicitNotFound("No member of type class ToBValue in scope for ${T}")
trait ToBValue[T] extends (T => BValue)

/** Precanned implicit conversions to [[BValue]].
 *
 *  @group bfactory
 */
object ToBValue {
  implicit def fromInt: ToBValue[Int] = BInt
  implicit def fromLong: ToBValue[Long] = BLong
  implicit def fromString: ToBValue[String] = BString
  implicit def fromSymbol: ToBValue[Symbol] = BSymbol
}

/** Implicit conversion to [[BBuilder]].
 *
 *  @group generating
 */
@implicitNotFound("No member of type class ToBBuilder in scope for ${T}")
trait ToBBuilder[T] extends (T => BBuilder)

/** Precanned implicit conversions to [[BBuilder]].
 *
 *  @group generating
 */
object ToBBuilder {

  /** Implicit conversion from a [[scala.collection.generic.Growable Growable[Byte]]] (such as an
   *  [[scala.collection.mutable.ArrayBuilder.ofByte ArrayBuilder[Byte]]]) to a BBuilder.
   */
  implicit def fromGrowable[T <: Growable[Byte]]: ToBBuilder[T] = {
    case growable => new BBuilder {
      def +=(b: Byte) = { growable += b; this } // scalastyle:ignore method.name
      override def ++=(bs: Array[Byte]) = { growable ++= bs; this }
    }
  }

  implicit def fromOutputStream[T <: java.io.OutputStream]: ToBBuilder[T] = {
    case os => new BBuilder {
      def +=(b: Byte) = { os.write(b.toInt); this } // scalastyle:ignore method.name
      override def ++=(bs: Array[Byte]) = { os.write(bs.toArray); this }
    }
  }

}

/** Implicit conversion to [[BIterator]].
 *
 *  @group parsing
 */
@implicitNotFound("No member of type class ToBIterator in scope for ${T}")
trait ToBIterator[T] extends (T => BIterator)

/** Precanned implicit conversions to [[BIterator]].
 *
 *  @group parsing
 */
object ToBIterator {

  implicit def fromByteIterator[T <: Iterator[Byte]]: ToBIterator[T] = {
    case it => new BIterator { def next() = it.next() }
  }

  implicit def fromInputStream[T <: java.io.InputStream]: ToBIterator[T] = {
    case is => new BIterator {
      def next() = is.read match {
        case -1    => throw new java.io.EOFException("short read: wanted a byte")
        case other => other.toByte
      }
      override def nextSeq(length: Int) = {
        val bs = new Array[Byte](length)
        is.read(bs) match {
          case `length` => bs
          case other    => throw new java.io.EOFException(s"short read: wanted $length but got $other")
        }
      }
    }
  }

  implicit def fromByteArray: ToBIterator[Array[Byte]] = {
    case bs => new BIterator {
      private[this] var nextIndex = 0
      def next() = {
        val index = nextIndex
        nextIndex += 1
        bs(index) // no need for a bounds-check as the array access does that
      }
      override def nextSeq(length: Int) = {
        val index = nextIndex
        nextIndex += length
        if (nextIndex > bs.length) {
          throw new ArrayIndexOutOfBoundsException(s"wanted $length but overran array by ${bs.length - index}")
        }
        bs slice (index, nextIndex)
      }
    }
  }

}
