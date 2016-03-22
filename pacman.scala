package pacman

import collection.immutable
import collection.mutable

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


object Game extends App {
	def drawGrid(grid: GameGrid) {
		for (i <- 0 to grid.range) {
			grid.spaces((i, 10))
		}
	}


	val ui = PacmanUI
	ui.draw(GameGrid(10))
}




