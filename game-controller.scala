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
import java.io.{ PrintStream, InputStream, OutputStream }
import io.BufferedSource

object SocketUtil {
  def handshake(inputStream: InputStream, outputStream: OutputStream) {

    def readHeaders(inputStream: InputStream): collection.mutable.Map[String, String] = {
      val headers = collection.mutable.Map[String, String]()

      val inputSource = new BufferedSource(inputStream)
      val inputLines = inputSource.getLines.drop(0)

      var hasMore = true 
      while (hasMore) {
        val header = inputLines.next()
        if (header.length > 2) {
          val headerName = header.split(":".charAt(0))(0)
          val headerValue = header.split(" ".charAt(0))(1)
          headers(headerName) = headerValue
        } else {
          hasMore = false
        }
      }
      headers
    }
    
    val webSocketAccept = {
      val webSocketKey = readHeaders(inputStream)("Sec-WebSocket-Key")

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
  }
  
  def sendMessage(stream: OutputStream, message: String) {
    stream.write(129.toByte)

    message.getBytes.length match {
      case length if length < 126 =>
        stream.write(length.toByte)
      case length if length > 126 && length < 65536 =>
        stream.write(126.toByte)
        stream.write((length >> 8 & 255).toByte)
        stream.write((length >> 0 & 255).toByte)
      case length if length > 65536 =>
        // TODO
        stream.write(0.toByte)
    }

    stream.write(message.getBytes)
  }

  def readMessage(stream: InputStream): String = {
    val messageStatusCode = stream.read()
    val lengthByte = stream.read()
    
    val messageLength = {
      if (lengthByte - 128 < 125) {
        lengthByte - 128
      } else if (lengthByte - 128 == 126) {
        // TODO
        stream.read() * stream.read()
      } else if (lengthByte - 128 == 127) {
        // TODO
        for (i <- 1 to 8) stream.read()
      }
    }

    val messageKey = List(stream.read(), stream.read(), stream.read(), stream.read())
    val messageEncoded = for (i <- 1 to stream.available) yield stream.read()
    val messageDecoded = for (i <- 0 to messageEncoded.length - 1) yield (messageEncoded(i) ^ messageKey(i % 4))

    new String(messageDecoded.map(_.toByte).map(_.toChar).toArray)
  }
}


object SocketServer extends App {
  val socket = new ServerSocket(8000)

  while (true) {
    val connection = socket.accept()
    println("Connection from new client received!")
    
    val inputStream = connection.getInputStream()
    val outputStream = connection.getOutputStream()
    
    SocketUtil.handshake(inputStream, outputStream)
   
    val game = new Game()

    // What kind of concurrency problems does this create for a client?
    val inputThread = new Thread(new Runnable {
      def run {
        while (true) {
          val message = SocketUtil.readMessage(inputStream)

          if (message == "") {
            println(s"Assuming message '$message' is kill signal")
            connection.close()
          }

          message match {
            case "East" => game.player.directionRequest = East
            case "West" => game.player.directionRequest = West
            case "North" => game.player.directionRequest = North
            case "South" => game.player.directionRequest = South
            case (key: String)  =>
              println(s"Recevied bad direction $key")
          }
        }
      }
    })
    inputThread.start()

    val outputThread = new Thread(new Runnable {
      def run {
        SocketUtil.sendMessage(outputStream, GameController.mazeToJson(game.maze))
        SocketUtil.sendMessage(outputStream, GameController.movablesToJson(game.movables))

        // Wait for the opening song.
        Thread.sleep(4000)
        
        game.start()

        while (true) {
          Thread.sleep(20)
          SocketUtil.sendMessage(outputStream, GameController.mazeToJson(game.maze))
          SocketUtil.sendMessage(outputStream, GameController.movablesToJson(game.movables))
          SocketUtil.sendMessage(outputStream, GameController.stateToJson(game.state))
        }
      }
    })
    outputThread.start()

  }
}

object GameController {
  // TODO: Can we make this even cleaner using only Java/Scala built-ins?
  def mazeToJson(maze: Map[(Int, Int), DrawableElement]): String = {
    "{" + {
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
    } + "}"
  }

  def movablesToJson(movables: Set[Movable]): String = {
    "{" + {
      "\"movables\":" + "[" + movables.map {
        case (movable: Movable) =>
          "{" + 
            "\"direction\":"        + s""""${movable.direction}"""" + "," +
            "\"directionRequest\":" + s""""${movable.directionRequest}"""" + "," + 
            "\"speed\":"            + movable.speed + "," + 
            "\"speedMs\":"          + movable.speedMs + "," + 
            "\"width\":"            + movable.width + "," + 
            "\"height\":"           + movable.height + "," + 
            "\"id\":"               + s""""${movable.id}"""" + "," + 
            "\"x\":"                + movable.x + "," +
            "\"y\":"                + movable.y + 
          "}"
      }.mkString(",") + "]"
    } + "}"
  }

  def stateToJson(state: GameState): String = {
    s"""
    {
      "state":"${
        state match {
          case PlayerPlaying => "player-playing"
          case PlayerWon => "player-won"
          case PlayerLost => "player-lost"
        }
      }"
    }
    """
  }
}

