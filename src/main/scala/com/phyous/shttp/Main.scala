package com.phyous.shttp

import java.net._
import java.io._

import org.slf4j.LoggerFactory

import scala.io._

object Main  {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  // http://www.masterraghu.com/subjects/np/introduction/unix_network_programming_v1.3/ch02lev1sec6.html
  def main(args: Array[String]): Unit = {
    val address = "www.google.com"
    val inetAddress = InetAddress.getByName("google.com")

    logger.debug(s"Connecting to $inetAddress")
    val s = new Socket(InetAddress.getByName("google.com"), 80)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())

    out.println("Hello, world")
    out.flush()
    println("Received: " + in.next())

    s.close()
  }

}
