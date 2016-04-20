package pacman

import collection.immutable
import collection.mutable

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
	val WallColor = Color.BLUE
	val PathwayColor = Color.BLACK
	val PlayerColor = Color.YELLOW

	val game = new Game()
	
	val tileSize = 20
	val tilesX = game.maze.map(_._1._1).max + 1// <3 Scala
	val tilesY = game.maze.map(_._1._2).max + 1

	println(tilesX, tilesY)

	val root = new Group()
	val scene = new Scene(root, tilesX * tileSize, tilesY * tileSize)

	override def start(primaryStage: javafx.stage.Stage) {
		primaryStage.setTitle("Hello world.")
		primaryStage.setScene(scene)
		primaryStage.show()
	}

	def drawMaze(maze: immutable.Map[(Int,Int), DrawableElement]) {
		root.getChildren.clear()

		maze.foreach { tile =>
			val coordinates = tile._1
			val element = tile._2

			val rect = new Rectangle(tileSize, tileSize, element match {
					case _: Wall => this.WallColor
					case _: Space => this.PathwayColor
					case _: Filled => this.PathwayColor
				})
			rect.setX(coordinates._1 * tileSize)
			rect.setY(coordinates._2 * tileSize)
			root.getChildren.add(rect)	
		}
	}

	val movablesById = mutable.Map[String, Circle]()

	def drawMovables(movables: immutable.Set[Movable]) {
		movables.foreach { (elem: Movable) =>
			if (movablesById.isDefinedAt(elem.id) == false) {
				movablesById(elem.id) = new Circle()
				movablesById(elem.id).setRadius(10)
                movablesById(elem.id).setFill(PlayerColor)
				root.getChildren.add(movablesById(elem.id))
			}
			movablesById(elem.id).setCenterX(elem.x * tileSize)
			movablesById(elem.id).setCenterY(elem.y * tileSize)
		}
	}


    val consumablesByCoordinates = mutable.Map[(Int, Int), Circle]()
    def drawConsumables(maze: immutable.Map[(Int,Int), DrawableElement]) {
        maze.foreach { case ((coordinates: (Int, Int), elem: DrawableElement)) => 
            elem match {
                case _: Wall =>
                case _: Filled =>
                case space: Space => 
                    if (space.consumableAvailable && consumablesByCoordinates.isDefinedAt(coordinates) == false) {

                        consumablesByCoordinates(coordinates) = new Circle()
                        consumablesByCoordinates(coordinates).setFill(PlayerColor)

                        consumablesByCoordinates(coordinates).setCenterX((coordinates._1 + 0.5) * tileSize)
                        consumablesByCoordinates(coordinates).setCenterY((coordinates._2 + 0.5) * tileSize)

                        space.consumable match {
                            case _: Dot =>
                                consumablesByCoordinates(coordinates).setRadius(3)
                            case _: PowerPellet =>
                                consumablesByCoordinates(coordinates).setRadius(5)
                        }

                        root.getChildren.add(consumablesByCoordinates(coordinates))

                    } else if (consumablesByCoordinates.isDefinedAt(coordinates) && space.consumableAvailable == false) {
                        root.getChildren.remove(consumablesByCoordinates(coordinates))
                        consumablesByCoordinates -= coordinates
                    }
            }
		}
    }

	val loopSleepDurationMilliseconds = 10

	def loop(i: Integer) {
		val task = new Task[Unit] {
			override def call(): Unit = {
				Thread.sleep(loopSleepDurationMilliseconds)
			}
			override def succeeded() {
				drawMovables(game.world.movables)
				drawConsumables(game.maze)
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

	drawMaze(game.maze)
    drawConsumables(game.maze)
	loop(0)
}


