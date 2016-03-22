package pacman

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.shape
import scalafx.scene.paint.Color
import scalafx.scene.control.Label
import scalafx.scene.layout.BorderPane

object PacmanUI extends JFXApp {
	val game = GameGrid(100)
	
	def draw(gameGrid: GameGrid): collection.immutable.Iterable[shape.Rectangle] = {
		gameGrid.spaces.map {  tile =>
			val coordinates = tile._1
			val element = tile._2

			new shape.Rectangle {
				width = 20
				height = 20
			
				fill = element match {
					case _: Wall => Color.WHITE
					case _: Space => Color.BLUE
					case _: Filled => Color.BLACK
				}

				x = coordinates._1 * 20
				y = coordinates._2 * 20
			}
		}
	}

	stage = new PrimaryStage {
		title.value = "Hello Stage"
		width = 600
		height = 450
		scene = new Scene {
			content = draw(game)
		}
	}
}
