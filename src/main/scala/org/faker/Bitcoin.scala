package org.faker

import java.security.MessageDigest
import scala.util.Random
import scala.annotation.tailrec

/**
 * generates bitcoin addresses
 *
 * A bitcoin address is in fact the hash of a ECDSA public key, computed this way:
 *
 * Version = 1 byte of 0 (zero); on the test network, this is 1 byte of 111 (we will always use '0' zero)
 *
 * Key hash = Version concatenated with RIPEMD-160(SHA-256(public key))
 *
 * Checksum = 1st 4 bytes of SHA-256(SHA-256(Key hash))
 *
 * Bitcoin Address = Base58Encode(Key hash concatenated with Checksum)
 *
 */
object Bitcoin extends Base {
  val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toArray
  val base = alphabet.length
  val mac = MessageDigest.getInstance("SHA-256")

  /**
   * returns a bitcoin address
   */
  def address: String = {
    val byteStream: Stream[Byte] = Stream.continually(Random.nextInt().toByte)
    val byteArray: Array[Byte] = (0.toByte #:: byteStream.take(20)).toArray
    // due to https://en.bitcoin.it/wiki/Protocol_specification#Hashes the checksum is four bytes long
    // (not three as in the ruby implementaion)
    val checkSum = sha2(byteArray).take(4)
    base58(byteArray ++ checkSum)
  }

  private def base58(a: Array[Byte]): String = {
    @tailrec
    def dec(n: BigInt, acc: List[Char]): String = {
      if (n == 0)
        acc.mkString
      else {
        val div = n / base
        val mod = n % base
        val c = alphabet(mod.intValue())
        dec(div, c :: acc)
      }
    }

    dec(BigInt(1, a), Nil)
  }

  private def sha2(a: Array[Byte]): Array[Byte] = mac.digest(a)
}
