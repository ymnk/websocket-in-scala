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

import java.net.Socket
import java.io.{InputStream, OutputStream}
import java.net.URI

import WebSocket._

class WebSocketClient(uri: URI)
                     (f: PartialFunction[Event, Unit]) extends WebSocket {
  import WebSocketClient._
 
  val path = (uri.getPath match{
    case "" => "/"
    case p => p
  }) + Option(uri.getQuery).map("?"+_).getOrElse("")

  val default_port = uri.getScheme match {
    case "ws" => 80
    case "wss" => 443
    case _ => error("unknown scheme: "+uri)
  } 

  val port = uri.getPort match{
    case -1 => default_port
    case p => p
  }

  val host = {
    uri.getHost + (port match{
      case `default_port` => ""
      case p => ":"+p
    })
  }

  val origin = "http://"+uri.getHost

  def connect(): Boolean = { 
    val s = {
      if(uri.getScheme == "wss"){
        import javax.net.SocketFactory
        import javax.net.ssl.SSLSocketFactory
        val factory = SSLSocketFactory.getDefault
	factory.createSocket(uri.getHost, port)
      }
      else{
        new Socket(uri.getHost, port)
      } 
    }

    in = s.getInputStream
    out = s.getOutputStream

    if(handshake()){
      import scala.concurrent.ops.spawn
      spawn{
        receive(f)
      }
    }
    connected
  }

  def handshake(): Boolean = {
    val key1 = genKey
    val key2 = genKey
    val key3 = {
      val buf = new Array[Byte](8)
      scala.util.Random.nextBytes(buf)
      buf
    }
    val request = template.format(path, host, origin, key1, key2)
    out.write(request.getBytes("UTF-8"))
    out.write(key3)
    out.flush

    val (head, headers) = readHeader

    val reply_digest = new Array[Byte](16)
    in.read(reply_digest)

    if(headers("Sec-WebSocket-Origin") != origin){
      println("warning: "+headers("Sec-WebSocket-Origin")+" != "+origin)
    }

    //connected = (reply_digest.deep == digest(key1, key2, key3).deep)
    connected = java.util.Arrays.equals(reply_digest, digest(key1, key2, key3))

    if(!connected){
      println("warning: wrong digest")
    }

    if(connected && f.isDefinedAt(OnOpen)) f(OnOpen)

    connected
  }
}

object WebSocketClient {
  val template = 
    "GET %s HTTP/1.1\r\n" +
    "Upgrade: WebSocket\r\n" +
    "Connection: Upgrade\r\n" +
    "Host: %s\r\n" +
    "Origin: %s\r\n" +
    "Sec-WebSocket-Key1: %s\r\n" +
    "Sec-WebSocket-Key2: %s\r\n" +
    "\r\n"

  private val characters = ((0x21 to 0x2F) toList) ::: ((0x3A to 0x7E) toList) map {
    _.asInstanceOf[Char]
  }

  def genKey = {
    import scala.util.Random.nextInt
    val spaces = nextInt(12)+1
    val max = ((BigInt("4294967295")  / spaces).toInt) & 0x7FFFFFFF
    val number = nextInt(max)
    val product = BigInt(number) * spaces
    def nextChar = {
      characters(nextInt(characters.length))
    }

    var key = product.toString

    key = (0 to nextInt(12)).foldLeft(key) {
      case (key, _) => 
        key splitAt nextInt(key.length) match {
          case (a,b) => a+nextChar+b
        }
      }

    key = (0 to spaces).foldLeft(key) {
      case (key, _) => 
        key splitAt nextInt(key.length-1)+1 match {
         case (a,b) => a+' '+b
      }
    }
    key
  }
}
