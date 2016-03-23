package pacman

import collection.immutable
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent

object GridMethods {
	def createGrid(n: Int): immutable.Map[(Int,Int), Any] = {
		{
			for (row <- 0.to(n); col <- 0.to(n)) yield { (row, col) -> None }
		}.toMap
	}
}

class DrawableElement()

class NonCollidable() extends DrawableElement
class Collidable() extends DrawableElement

case class Space() extends NonCollidable
case class Wall() extends Collidable
case class Filled() extends Collidable

case class GameGrid(val range: Integer) {
	val grid = GridMethods.createGrid(range)

	val spaces = grid.map { tile =>
		if (tile._1._1 == 2) {
			(tile._1 -> Filled())
		} else {
			(tile._1 -> Space())
		}
	}
}

object PacmanInput {

	sealed trait Key

	case object DOWN extends Key
	case object UP extends Key
	case object LEFT extends Key
	case object RIGHT extends Key

	def getKey(ev: KeyEvent): PacmanInput.Key = {
		ev.getCode match {
			case KeyCode.DOWN => DOWN
			case KeyCode.UP => UP
			case KeyCode.LEFT => LEFT
			case KeyCode.RIGHT => RIGHT
		}
	}
}


class Game {
	def keyPressed(ev: KeyEvent) {
		PacmanInput.getKey(ev) match {
			case PacmanInput.DOWN => println("down")
			case PacmanInput.UP => println("up")
			case PacmanInput.RIGHT => println("right")
			case PacmanInput.LEFT => println("left")
		}
	}

	def keyReleased(ev: KeyEvent) {}

	def drawGrid(grid: GameGrid) {
		for (i <- 0 to grid.range) {
			grid.spaces((i, 10))
		}
	}
}

object Game extends App {

	javafx.application.Application.launch(classOf[PacmanUI]);
	//ui.drawGame(GameGrid(10))
	//ui.start(new javafx.stage.PrimaryStage())
}




