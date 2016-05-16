// From my analysis of Pacman UI, it appears that the web client needs to do the following:
//
// PULL: 
//    - game.state
//    - game.maze
//    - game.movables
//
// PUSH:
//    - game.start()
//    - game.keyPress()
//

package pacman

import java.net.ServerSocket
import java.io.PrintStream
import io.BufferedSource

object GameController extends App {
  val socket = new ServerSocket(8000)

  val connection = socket.accept()
  
  val inputStream = connection.getInputStream()
  val outputStream = connection.getOutputStream()
 
  val inputSource = new BufferedSource(inputStream)
  val inputLines = inputSource.getLines

  val protocol = inputLines.next()
  val headers = collection.mutable.Map[String, String]()
  
  var hasMore = true

  while (hasMore) {
    val header = inputLines.next()
    println(header)

    if (header.length > 2) {
      val headerName = header.split(":".charAt(0))(0)
      val headerValue = header.split(" ".charAt(0))(1)
   
      headers(headerName) = headerValue
    } else {
      hasMore = false
    }
  }

  val webSocketKey = headers("Sec-WebSocket-Key")
  
  val webSocketAccept = {
    java.util.Base64.getEncoder.encodeToString {
      java.security.MessageDigest
        .getInstance("SHA-1")
        .digest(s"${webSocketKey}258EAFA5-E914-47DA-95CA-C5AB0DC85B11".getBytes)
    }
  }

  val out = new PrintStream(outputStream)

  out.println("HTTP/1.1 101 Switching Protocols")
  out.println("Upgrade: websocket")
  out.println("Connection: Upgrade")
  out.println(s"Sec-WebSocket-Accept: ${webSocketAccept}")
  out.println("")
  out.flush()

  inputStream.getClass.getMethods.foreach(println(_))

  def sendMessage(stream: java.io.OutputStream, message: String) = {
    stream.write(129.toByte)

    message.getBytes.length match {
      case length if length < 126 =>
        stream.write(length.toByte)
      case length if length > 126 && length < 65536 =>
        stream.write(126.toByte)
        println(s"length: $length")
        stream.write((length >> 8 & 255).toByte)
        stream.write((length >> 0 & 255).toByte)
      case length if length > 65536 =>
        stream.write(127.toByte)
    }

    stream.write(message.getBytes)
  }

  def readMessage(stream: java.io.InputStream): String = {
    val fin = stream.read()

    val lengthByte = stream.read()
    
    val messageLength = {
      if (lengthByte - 128 < 125) {
        lengthByte - 128
      // TODO: These cases are wrong.
      } else if (lengthByte - 128 == 126) {
        stream.read() * stream.read()
      } else if (lengthByte - 128 == 127) {
        for (i <- 1 to 8) stream.read()
      }
    }

    val messageKey = List(stream.read(), stream.read(), stream.read(), stream.read())
    val messageEncoded = for (i <- 1 to stream.available) yield stream.read()
    val messageDecoded = for (i <- 0 to messageEncoded.length - 1) yield (messageEncoded(i) ^ messageKey(i % 4))

    println(s"Message length: $messageLength")
    println(s"Message key: $messageKey")
    println(s"Message encoded: $messageEncoded")
    println(s"Message decoded: $messageDecoded")
    new String(messageDecoded.map(_.toByte).map(_.toChar).toArray)

  }

  
  val game = new Game()

  def sendMaze(stream: java.io.OutputStream, maze: Map[(Int, Int), DrawableElement]) {
    var json = "{"
    json += {
      "\"maze\":" + "[" + maze.map {
        case ((column: Int, row: Int), skin: Space) =>
          "{" + 
            "\"row\":" + row.toString + "," +
            "\"column\":" + column.toString + "," + 
            "\"consumableAvailable\":" + skin.consumableAvailable.toString + "," + 
            "\"skin\":" + "\"space\""  +
          "}"
        case ((column: Int, row: Int), skin: Wall) =>
          "{" + 
            "\"row\":" + row.toString + "," +
            "\"column\":" + column.toString + "," + 
            "\"skin\":" + "\"wall\""  +
          "}"
      }.mkString(",") + "]"
    }
    json += "}"

    sendMessage(stream, json)
  }
//
//  def sendMovables(stream: java.io.OutputStream, movables: Set[Movable]) {
//    var json = "{"
//    json += {
//      "\"maze\":" + "[" + maze.map {
//        case ((column: Int, row: Int), skin: Space) =>
//          "{" + "\"row\":" + row.toString + "," + "\"column\":" + column.toString + "," + "\"skin\":" + "\"space\""  + "}"
//        case ((column: Int, row: Int), skin: Wall) =>
//          "{" + "\"row\":" + row.toString + "," + "\"column\":" + column.toString + "," + "\"skin\":" + "\"wall\"" + "}"
//      }.mkString(",") + "]"
//    }
//    json += "}"
//  }

  sendMaze(outputStream, game.maze)
  //sendMovables(outputStream, game.maze)

  while (true) {}

  socket.close()
}

