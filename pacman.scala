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
					case 'X' => Space()
					case '-' => Wall()
				}

				// X, Y	
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
	var direction: Direction
	var x: Int
	var y: Int

	// Note: speed in x is the same as speed in y.
	val speed: Int
	val speedMs = speed / 1000

	// NOTE: Would be cool to make this work with arbitrary curves.
	val width: Int
	val height: Int
}

class Ghost(var x: Int, var y: Int) extends Movable {
	var direction: Direction = immutable.Set[Direction](East, West, North, South).last
	val speed = 1
	val width = 1
	val height = 1
}

class Player(var x: Int, var y: Int) extends Movable {
	var direction: Direction = East
	val speed = 1
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

	def nudgeMovable(elem: Movable, timeMs: Int) {
		val direction = elem.direction

		direction match {
			case East => elem.x = elem.x + elem.speed * timeMs
			case West => elem.x = elem.x - elem.speed * timeMs
			case North => elem.y = elem.y + elem.speed * timeMs
			case South => elem.y = elem.y - elem.speed * timeMs
		}
	}
	
	def canNudgeMovable(elem: Movable) {}

	// But in the real world, all things move (or don't) at the same time.
	// map.movables.foreach
	def update(elapsedTimeMs: Int) {
		(0 to elapsedTimeMs).foreach { _ =>
			movables.map { m => nudgeMovable(m, 1) }
		} 
	}
}

class Game {
	val world = new World()
	val maze = world.map
	val mazeGrid = world.map.grid

	val player = new Player(x = 0, y = 0)
	world.addMovable(player)

	def keyPressed(ev: KeyEvent) {
		this.player.direction = PacpersonInput.getKey(ev) match {
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


