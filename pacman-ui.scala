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
	val game = new Game()
	
	val tileSize = 20
	val tilesX = game.maze.width
	val tilesY = game.maze.height

	val root = new Group()
	val scene = new Scene(root, tilesX * tileSize, tilesY * tileSize)

	override def start(primaryStage: javafx.stage.Stage) {
		primaryStage.setTitle("Hello world.")
		primaryStage.setScene(scene)
		primaryStage.show()
	}

	def drawMaze(mazeGrid: immutable.Map[(Int,Int), DrawableElement]) {
		root.getChildren.clear()

		mazeGrid.map { tile =>
			println(tile)
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

	val loopSleepDurationMilliseconds = 10

	def loop(i: Integer) {
		val task = new Task[Unit] {
			override def call(): Unit = {
				Thread.sleep(loopSleepDurationMilliseconds)
			}
			override def succeeded() {
				loop(i + 1)
			}
		}
		val t = new Thread(task, s"frame-{i}")
		t.setDaemon(true)
		t.start()
	}

	scene.addEventHandler(KeyEvent.KEY_PRESSED, new EventHandler[KeyEvent] {
		override def handle(ev: KeyEvent) {
			game.keyPressed(ev)
		}
	})

	drawMaze(game.mazeGrid)
	loop(0)
}


