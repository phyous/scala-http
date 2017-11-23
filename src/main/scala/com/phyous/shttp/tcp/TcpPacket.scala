package com.phyous.shttp.tcp

import java.io.ByteArrayOutputStream

import com.phyous.shttp.util.dsbyte._

// https://en.wikipedia.org/wiki/Transmission_Control_Protocol
// http://www.freesoft.org/CIE/Course/Section4/8.htm
case class TcpPacket(sourcePort: Int,
                     destPort: Int,
                     sequenceNumber: Int,
                     ackNumber: Int,
                     dataOffset: Int,
                     setURG: Boolean,
                     setACK: Boolean,
                     setPSH: Boolean,
                     setRST: Boolean,
                     setSYN: Boolean,
                     setFIN: Boolean,
                     window: Int,
                     urgentPointer: Int,
                     options: Array[Byte],
                     data: Array[Byte]) {

  def toByteStream:Array[Byte] = {
    val template = s"<<'$sourcePort:2, '$destPort:2>>"
    val parser = new DSByteParser()
    val pu = parser.parseAll(parser.template,template).get
    val bos = new ByteArrayOutputStream()
    pu.packValues(bos)

    bos.toByteArray
  }
}

case class TcpPacketError(reason: String)

case class TcpPacketBuilder(sourcePort:Int, destPort:Int) {
  var sequenceNumber:Int = 0
  var ackNumber:Int = 0
  var dataOffset:Int = 0
  var setURG:Boolean = false
  var setACK:Boolean = false
  var setPSH:Boolean = false
  var setRST:Boolean = false
  var setSYN:Boolean = false
  var setFIN:Boolean = false
  var window:Int = 0
  var urgentPointer:Int = 0
  var options:Array[Byte] = Array()
  var data:Array[Byte] = Array()

  def sequenceNumber(v: Int):TcpPacketBuilder = {this.sequenceNumber = v; this}
  def ackNumber(v: Int):TcpPacketBuilder = {this.ackNumber = v; this}
  def setURG(v: Boolean):TcpPacketBuilder = {this.setURG = v; this}
  def setACK(v: Boolean):TcpPacketBuilder = {this.setACK = v; this}
  def setPSH(v: Boolean):TcpPacketBuilder = {this.setPSH = v; this}
  def setRST(v: Boolean):TcpPacketBuilder = {this.setRST = v; this}
  def setSYN(v: Boolean):TcpPacketBuilder = {this.setSYN = v; this}
  def setFIN(v: Boolean):TcpPacketBuilder = {this.setFIN = v; this}
  def window(v: Int):TcpPacketBuilder = {this.window = v; this}
  def urgentPointer(v: Int):TcpPacketBuilder = {this.urgentPointer = v; this}
  def options(v: Array[Byte]):TcpPacketBuilder = {this.options = v; this}
  def data(v: Array[Byte]):TcpPacketBuilder = {this.data = v; this}

  val validations: List[() => Option[TcpPacketError]] = List(
    () => if (this.sourcePort < 0 || this.sourcePort > Short.MaxValue) Some(TcpPacketError(s"sourcePort value $sourcePort is invalid")) else None,
    () => if (this.destPort < 0 || this.destPort > Short.MaxValue) Some(TcpPacketError(s"destPort value $destPort is invalid")) else None
  )

  val fieldBuilders = List(
    () => {
      if (this.setSYN) {
        sequenceNumber = 0
      } else {
        dataOffset = 0
      }
    }
  )

  def build: Either[List[TcpPacketError], TcpPacket] = {
    val errors = validations.flatMap(x => x.apply())
    if (errors.isEmpty)
      Right(TcpPacket(this.sourcePort,
        this.destPort,
        this.sequenceNumber,
        this.ackNumber,
        this.dataOffset,
        this.setURG,
        this.setACK,
        this.setPSH,
        this.setRST,
        this.setSYN,
        this.setFIN,
        this.window,
        this.urgentPointer,
        this.options,
        this.data))
    else Left(errors)
  }

}
