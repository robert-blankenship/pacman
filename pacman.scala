package pacman

import collection.immutable
import collection.mutable
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent

class DrawableElement()

class NonCollidable() extends DrawableElement
class Collidable() extends DrawableElement

case class Space() extends NonCollidable
case class Wall() extends Collidable
case class Filled() extends Collidable

sealed trait Direction
case object East extends Direction
case object West extends Direction
case object North extends Direction
case object South extends Direction

trait Movable extends Collidable {
	// NOTE: Is immutability possible here?
	val id = java.util.UUID.randomUUID.toString

	var direction: Direction
	var directionRequest: Direction

	// This is for the center of the rectangle.
	var x: Double
	var y: Double

	// Makes the code more unstable...
	// But, I don't have any better ideas
	def atCenterOfTile: Boolean = {
		def check(r: Double): Boolean = {
			(r * 1000).toInt - (r.toInt * 1000) == 500 ||
			(r * 1000).toInt - (r.toInt * 1000) == 499 ||
			(r * 1000).toInt - (r.toInt * 1000) == 501
		}

		check(x) && check(y)
	} 

	// Note: speed in x is the same as speed in y.
	val speed: Double = 4.0
	val speedMs: Double = speed / 1000

	// NOTE: Would be cool to make this work with arbitrary curves.
	val width: Int
	val height: Int
}

class Ghost(var x: Double, var y: Double) extends Movable {
	var direction: Direction = immutable.Set[Direction](East, West, North, South).last
	var directionRequest: Direction = direction
	val width = 1
	val height = 1
}

class Player(var x: Double, var y: Double) extends Movable {
	var direction: Direction = East
	var directionRequest: Direction = direction
	val width = 1
	val height = 1
}


class World {
	def createMaze(mapLines: List[String]): immutable.Map[(Int, Int), DrawableElement] = {
		mapLines.zipWithIndex.map { (row: Tuple2[String, Int]) =>
			val rowString = row._1
			val rowIndex = row._2

			rowString.zipWithIndex.map { (tile: Tuple2[Char, Int]) =>
				val columnIndex = tile._2

				val tileType = tile._1 match {
					case 'X' => Wall()
					case '-' => Space()
				}

				(columnIndex, rowIndex) -> tileType
			}
		}.toIndexedSeq.flatten.toMap
	}

	var movables = immutable.Set[Movable]()

	def getAllowedDirections(elem: Movable, maze: immutable.Map[(Int, Int), DrawableElement]): immutable.Set[Direction] = {
		// This makes the code a little unstable, but we'll put up with it for now :).
		if (elem.atCenterOfTile) {
			// Is in direction of blah... would be useful here.
			def tileExists(coordinateX: Int, coordinateY: Int): Boolean = {
				maze.contains(coordinateX, coordinateY)
			}

			def tileIsSpace(coordinateX: Int, coordinateY: Int): Boolean = {
				maze(coordinateX, coordinateY) match {
					case _:Space => true
					case _       => false
				} 
			}

			def tileExistsAndIsSpace(coordinateX: Int, coordinateY: Int): Boolean = {
				tileExists(coordinateX, coordinateY)  && tileIsSpace(coordinateX, coordinateY)
			}
			
			var allowedDirections = immutable.Set[Direction]()

			if (tileExistsAndIsSpace(elem.x.toInt + 1, elem.y.toInt)) allowedDirections = allowedDirections + East
			if (tileExistsAndIsSpace(elem.x.toInt - 1, elem.y.toInt)) allowedDirections = allowedDirections + West
			if (tileExistsAndIsSpace(elem.x.toInt, elem.y.toInt + 1)) allowedDirections = allowedDirections + South
			if (tileExistsAndIsSpace(elem.x.toInt, elem.y.toInt - 1)) allowedDirections = allowedDirections + North

            val mazeLength = maze.map(_._1._2).max

            if (elem.x.toInt > mazeLength && tileExistsAndIsSpace(0, elem.y.toInt)) allowedDirections = allowedDirections + East
            if ((elem.x - 0.01).toInt == 0 && tileExistsAndIsSpace(mazeLength - 1, elem.y.toInt)) allowedDirections = allowedDirections + West

			allowedDirections
		} else {
			if (immutable.Set[Direction](East, West).contains(elem.direction)) {
				immutable.Set[Direction](East, West)
			} else {
				immutable.Set[Direction](North, South)
			}
		}
	}

	// But in the real world, all things move (or don't) at the same time.
	def update(maze: immutable.Map[(Int, Int), DrawableElement], elapsedTimeMs: Int) {
		(0 to elapsedTimeMs).foreach { _ =>
			this.movables.foreach { elem =>
				val allowedDirections = getAllowedDirections(elem, maze)
				
				if (elem.direction != elem.directionRequest && allowedDirections.contains(elem.directionRequest)) {
					elem.direction = elem.directionRequest
				}

				if (allowedDirections.contains(elem.direction)) {
					elem.direction match {
						case East => elem.x = elem.x + elem.speedMs * 1
						case West => elem.x = elem.x - elem.speedMs * 1
						case South => elem.y = elem.y + elem.speedMs * 1
						case North => elem.y = elem.y - elem.speedMs * 1
					}
				}

                val mazeLength = maze.map(_._1._2).max

                if (elem.x > mazeLength || elem.x < 0) {
                    elem.x = mazeLength - elem.x.abs
                }
			}
		} 
	}
}

class Game {
	val world = new World()

	val maze = {
		val mapSource = scala.io.Source.fromFile("mazes/dev.txt")
		world.createMaze(mapSource.getLines().toList) 
	}

	val player = new Player(x = 0.5, y = 0.5)
	val ghost1 = new Ghost(x = 10.5, y = 10.5)

	world.movables += player
	world.movables += ghost1

	def keyPressed(ev: KeyEvent) {
		this.player.directionRequest = ev.getCode match {
			case KeyCode.DOWN => South
			case KeyCode.UP => North
			case KeyCode.RIGHT => East
			case KeyCode.LEFT => West
		}
	}

	val gameThread = {
		// Boilerplate
		val loopSleepDurationMilliseconds = 10
		val gameLoop = new Runnable {
			def run {
				while (true) {
					Thread.sleep(loopSleepDurationMilliseconds)
					world.update(maze, loopSleepDurationMilliseconds)
				}
			}
		}
		new Thread(gameLoop)
	}

	gameThread.start()
}

object Game extends App {
	javafx.application.Application.launch(classOf[PacmanUI]);
}

