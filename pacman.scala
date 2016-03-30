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

class Maze(val mapSource: scala.io.Source) {
	val mapLines = mapSource.getLines().toList

	val height = mapLines.length
	val width = mapLines.apply(1).length
	
	val grid: immutable.Map[(Int, Int), DrawableElement] = {
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
}

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

		val res = check(x) && check(y)
		if (res) println(res)
		res
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


class World extends {
	val map = new Maze(scala.io.Source.fromFile("mazes/dev.txt"))

	var movables = immutable.Set[Movable]()

	def addMovable(elem: Movable) {
		movables = movables + elem
	}

	def removeMovable(elem: Movable) {
		movables = movables - elem
	}

	def getAllowedDirections(elem: Movable): immutable.Set[Direction] = {
		// This makes the code a little unstable, but we'll put up with it for now :).
		if (elem.atCenterOfTile) {
			// Is in direction of blah... would be useful here.
			def tileExists(coordinateX: Int, coordinateY: Int): Boolean = {
				map.grid.contains(coordinateX, coordinateY)
			}

			def tileIsSpace(coordinateX: Int, coordinateY: Int): Boolean = {
				map.grid(coordinateX, coordinateY) match {
					case _:Space => true
					case _     => false
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

			allowedDirections

		} else {
			if (immutable.Set[Direction](East, West).contains(elem.direction)) {
				immutable.Set[Direction](East, West)
			} else {
				immutable.Set[Direction](North, South)
			}
		}
	}

	def nudgeMovable(elem: Movable, timeMs: Int) {
		val allowedDirections = getAllowedDirections(elem)

		if (elem.direction != elem.directionRequest && allowedDirections.contains(elem.directionRequest)) {
			elem.direction = elem.directionRequest
		}

		if (allowedDirections.contains(elem.direction)) {
			elem.direction match {
				case East => elem.x = elem.x + elem.speedMs * timeMs
				case West => elem.x = elem.x - elem.speedMs * timeMs
				case South => elem.y = elem.y + elem.speedMs * timeMs
				case North => elem.y = elem.y - elem.speedMs * timeMs
			}
		}
	}

	// But in the real world, all things move (or don't) at the same time.
	// map.movables.foreach
	def update(elapsedTimeMs: Int) {
		(0 to elapsedTimeMs).foreach { _ =>
			movables.foreach { m => nudgeMovable(m, 1) }
		} 
	}
}

class Game {
	val world = new World()
	val maze = world.map
	val mazeGrid = world.map.grid

	val player = new Player(x = 0.5, y = 0.5)
	world.addMovable(player)

	def keyPressed(ev: KeyEvent) {
		this.player.directionRequest = PacpersonInput.getKey(ev) match {
			case PacpersonInput.DOWN => South
			case PacpersonInput.UP => North
			case PacpersonInput.RIGHT => East
			case PacpersonInput.LEFT => West
		}
	}

	val loopSleepDurationMilliseconds = 10
	val loop: Thread = new Thread {
		override def run {
			while (true) {
				Thread.sleep(loopSleepDurationMilliseconds)
				world.update(loopSleepDurationMilliseconds)
			}
		}
	}

	loop.start()
}

object PacpersonInput {
	sealed trait Key
	case object DOWN extends Key
	case object UP extends Key
	case object LEFT extends Key
	case object RIGHT extends Key
	def getKey(ev: KeyEvent): PacpersonInput.Key = {
		ev.getCode match {
			case KeyCode.DOWN => DOWN
			case KeyCode.UP => UP
			case KeyCode.LEFT => LEFT
			case KeyCode.RIGHT => RIGHT
		}
	}
}

object Game extends App {
	javafx.application.Application.launch(classOf[PacmanUI]);
}


