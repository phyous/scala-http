package com.phyous.shttp.tcp

import org.scalatest.FunSuite

class TcpPacketSuite extends FunSuite {

  def byteLineAsString(bytes: Array[Byte], from: Int, to: Int): String = {
    Range(from, to)
      .map(x =>
        String.format("%8s", Integer.toBinaryString(bytes(x)))
          .replace(' ', '0').reverse.substring(0,8).reverse
      )
      .foldLeft(new StringBuilder())((r, c) => r.append(c))
      .toString()
      .map(x => s"|$x")
      .foldLeft(new StringBuilder())((r, c) => r.append(c))
      .toString()
  }

  def printByteStream(bytes:Array[Byte]): Unit = {
    val sb = new StringBuilder()
    sb.append("|0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F|0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F|\n")
    sb.append("| source port                   | destination port              |\n")
    sb.append(byteLineAsString(bytes, 0, 4))
    sb.append("|                     sequence number                           |\n")
    sb.append(byteLineAsString(bytes, 4, 8))
    sb.append("|                 acknowledgment number                         |\n")
    sb.append(byteLineAsString(bytes, 8, 12))

    println(sb.toString())
  }

  test("test unpack") {
    TcpPacketBuilder(123,456).build match {
      case Left(errors) => fail(errors.toString())
      case Right(packet) => printByteStream(packet.toByteStream)
    }
  }
}
