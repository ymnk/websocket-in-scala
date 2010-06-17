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

import java.net.{Socket, ServerSocket}
import java.io.{InputStream, OutputStream}

import WebSocket._

class WebSocketServer(socket: Socket)
                     (f: PartialFunction[Event, Unit]) extends WebSocket {
  import WebSocketServer._

  in = socket.getInputStream
  out = socket.getOutputStream

  def connect(): Boolean = { 
    if(handshake()){
      import scala.concurrent.ops.spawn
      spawn{ receive(f) }
    }
    connected
  }

  def handshake(): Boolean = {
    val pattern = "GET (.+) HTTP/1.1".r
    val (pattern(path), headers) = readHeader

    val origin = headers("Origin")
    val host = headers("Host")
    val location = "ws://%s%s".format(host, path)
    val key1 = headers("Sec-WebSocket-Key1")
    val key2 = headers("Sec-WebSocket-Key2")
    val key3 = Array.fill(8)(read)
    val reply = template.format(
                  "101 Web Socket Protocol Handshake", 
                  origin, 
                  location,
                  ""
                )
    out.write(reply.getBytes("UTF-8"))
    out.write(digest(key1, key2, key3))
    out.flush

    connected = true

    println(connected)

    if(connected && f.isDefinedAt(OnOpen)) f(OnOpen)

    connected
  }
}

object WebSocketServer {
  val template = 
    "HTTP/1.1 %s\r\n" +
    "Upgrade: WebSocket\r\n" +
    "Connection: Upgrade\r\n" +
    "Sec-WebSocket-Origin: %s\r\n" +
    "Sec-WebSocket-Location: %s\r\n" +
    "%s\r\n"
}
