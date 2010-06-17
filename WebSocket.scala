/*
Copyright (c) 2010 ymnk, JCraft,Inc. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the distribution.

   3. The names of the authors may not be used to endorse or promote products
      derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JCRAFT,
 INC. OR ANY CONTRIBUTORS TO THIS SOFTWARE BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

import java.io.{InputStream, OutputStream}

trait WebSocket{
  import WebSocket._

  protected var in:InputStream = _
  protected var out:OutputStream = _
  protected var connected = false

  private val pattern = "(.+): (.+)".r
  protected def readHeader: (String, Map[String, String]) = {
    def oneLine = {
      val buf = new StringBuilder
      def loop(c: Int): String = {
        if(c==0x0d){in.read; buf.toString}
        else { buf += c.asInstanceOf[Char]; loop(in.read)}
      }
      loop(in.read)
    }
    val lines = for(l <- Stream continually (oneLine) takeWhile (_ != "")) yield l
    val headers = lines.tail.foldLeft(Map.empty[String,String]){ 
      case (m, pattern(k,v)) => m + (k -> v)
    } 
    (lines(0), headers)
  }

  protected def digest(key1:String, key2:String, key3:Array[Byte])={
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("MD5")
    digest.update(key2Int(key1))
    digest.update(key2Int(key2))
    digest.update(key3)
    digest.digest
  }

  private def key2Int(key: String): Array[Byte] = {
    val a = BigInt(key.toList.collect{case c if('0'<=c && c<='9')  => c}.mkString)
    val b = key.toList.filter( _ == ' ').length
    val i = (a / b).toInt
    val out = new java.io.ByteArrayOutputStream
    new java.io.DataOutputStream(out) match {
      case dos => dos.writeInt(i); dos.close
    }
    out.toByteArray
  }  

  protected def receive(f: PartialFunction[Event, Unit]) : Unit = {
    def loop(c:Int): Unit = c match {
      case -1 => error("IO exception")
      case c if (c&0x80)==0 =>
        val message = OnMessage(receiveMessage)
        if(f.isDefinedAt(message)) f(message)
        loop(in.read)
      case c =>
        val data = receiveData
        if(c== 0xff && data.length==0){
          // closing 
        }
        else{
          loop(in.read)
        }
    }
    loop(in.read)
  }

  protected def receiveMessage: String = {
    val buf = new StringBuilder
    def loop(c:Int): String = {
      if((c&0xff) == 0xff) buf.toString
      else { buf += c.asInstanceOf[Char]; loop(in.read) }
    }
    loop(in.read)
  }

  protected def receiveData: Array[Byte] = {
    def loop(c: Int, length:Int): Int = c match {
      case -1 => error("IO exception")
      case c => 
        length*128 + (c&0x7f) match {
          case l => 
            if((c&0x80) == 0x80) loop(in.read, l) else l
        }
    }
    val length = loop(in.read, 0)

    Array.fill(length)(read)
  }

  def send(data:String) = {
    out.write(0)
    out.write(data.getBytes("UTF-8"))
    out.write(0xff)
    out.flush
  }

  def close() = {
    out.write(0)
    out.write(0xff)
    out.flush
  }

  protected def dump(buf:Array[Byte])= {
    buf.foreach { c => print(Integer.toHexString(c&0xff)+":") }
    println("")
  }

  def read: Byte = {
    in.read match{
      case -1 => error("IO exception")
      case c => (c&0xff).asInstanceOf[Byte]
    }
  }

   /*
   val key1 = "P388 O503D&ul7 {K%gX( %7  15"
   val key2 = "1 N ?|k UT0or 3o  4 I97N 5-S3O 31"
   val key3 = List(0x47, 0x30, 0x22, 0x2D,
                   0x5A, 0x3F, 0x47, 0x58).map(_.asInstanceOf[Byte]).toArray
   // result = 0x30 0x73 0x74 0x33 0x52 0x6C 0x26 0x71
   //             0x2D 0x32 0x5A 0x55 0x5E 0x77 0x65 0x75
   // result == digest(key1, key2, key3)
   */
}

object WebSocket{
  abstract class Event
  case class OnMessage(message:String) extends Event
  // case class onMessage(message:Array[Byte]) extends Event
  case object OnOpen extends Event
  case object OnError extends Event
  case object OnClose extends Event
}
