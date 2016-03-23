package pacman

import collection.immutable

import javafx.concurrent.Task
import javafx.concurrent.WorkerStateEvent
import javafx.event.EventHandler
import javafx.application.Application
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.shape._
import javafx.scene.paint.Color
import javafx.scene.layout.Pane

import javafx.scene.input.KeyEvent


class PacmanUI extends Application {
	val tileSize = 20
	val tilesX = 20
	val tilesY = tilesX

	val root = new Group()
	val scene = new Scene(root, tilesX * tileSize, tilesY * tileSize)

	override def start(primaryStage: javafx.stage.Stage) {
		primaryStage.setTitle("Hello world.")
		primaryStage.setScene(scene)
		primaryStage.show()
	}

	def drawGame(gameGrid: GameGrid) {
		root.getChildren.clear()

		gameGrid.spaces.map {  tile =>
			val coordinates = tile._1
			val element = tile._2

			val rect = new Rectangle(tileSize, tileSize, element match {
					case _: Wall => Color.WHITE
					case _: Space => Color.BLUE
					case _: Filled => Color.BLACK
				})
			rect.setX(coordinates._1 * tileSize)
			rect.setY(coordinates._2 * tileSize)
			
			root.getChildren.add(rect)	
		}
	}

	def game_loop(i: Integer) {
		val task = new Task[Unit] {
			override def call(): Unit = {
				Thread.sleep(1000)
			}
			override def succeeded() {
				drawGame(GameGrid(i))
				game_loop(i + 1)
			}
		}
		val t = new Thread(task, s"frame-{i}")
		t.setDaemon(true)
		t.start()
	}

	val game = new Game()
	scene.addEventHandler(KeyEvent.KEY_PRESSED, new EventHandler[KeyEvent] {
		override def handle(ev: KeyEvent) {
			game.keyPressed(ev)
		}
	})

	game_loop(0)
}


