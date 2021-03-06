package pacman

import collection.immutable
import collection.mutable

import javafx.animation.AnimationTimer
import javafx.concurrent.Task
import javafx.concurrent.WorkerStateEvent
import javafx.event.EventHandler
import javafx.application.Application
import javafx.scene.{ Group, Scene, Node }
import javafx.scene.media.AudioClip
import javafx.scene.text.{ Text, TextAlignment }
import javafx.scene.shape._
import javafx.scene.paint.Color
import javafx.scene.layout.Pane
import javafx.scene.input.KeyEvent
import javafx.scene.media.{ MediaPlayer, Media }
import javafx.util.Duration


// Right now, it appears that PacmanUI has 3 dependencies in Game
//    - game.state
//    - game.maze
//    - game.movables
//
// It also calls the following
//    - game.start()
//    - game.keyPress()
class PacmanUI extends Application {
	val WallColor = Color.BLUE
	val PathwayColor = Color.BLACK
	val PlayerColor = Color.YELLOW
	val GhostColor = Color.PURPLE
    val PelletColor = PlayerColor

	val game = new Game()
	
	val tileSize = 20
	val tilesX = game.maze.map(_._1._1).max + 1
	val tilesY = game.maze.map(_._1._2).max + 1

	val root = new Group()
	val scene = new Scene(root, tilesX * tileSize, tilesY * tileSize)

	override def start(primaryStage: javafx.stage.Stage) {
		primaryStage.setTitle("Hello world.")
		primaryStage.setScene(scene)
		primaryStage.show()
	}

    // Watch for keypress.
	scene.addEventHandler(KeyEvent.KEY_PRESSED, new EventHandler[KeyEvent] {
		override def handle(ev: KeyEvent) = game.keyPressed(ev)
	})

    var mazeRectangles = List[Rectangle]()
	def drawMaze(maze: immutable.Map[(Int,Int), DrawableElement]) {
      root.getChildren.clear()
      mazeRectangles = maze.map { tile =>
        val tileColor = tile._2 match {
          case _: Wall => this.WallColor
          case _: Space => this.PathwayColor
          case _: Filled => this.PathwayColor
        }

        val rect = new Rectangle(tileSize, tileSize, tileColor) 
        val coordinates = tile._1
        rect.setX(coordinates._1 * tileSize)
        rect.setY(coordinates._2 * tileSize)
        root.getChildren.add(rect)
        rect
      }.toList
	}

	val movablesById = mutable.Map[String, Circle]()
	def drawMovables(movables: immutable.Set[Movable]) {
		movables.foreach { (elem: Movable) =>
			if (movablesById.isDefinedAt(elem.id) == false) {
				movablesById(elem.id) = new Circle()
				movablesById(elem.id).setRadius(10)
                elem match {
                  case _: Player => movablesById(elem.id).setFill(PlayerColor)
                  case _: Ghost => movablesById(elem.id).setFill(GhostColor)
                }
				root.getChildren.add(movablesById(elem.id))
			}
			movablesById(elem.id).setCenterX(elem.x * tileSize)
			movablesById(elem.id).setCenterY(elem.y * tileSize)
		}
	}


    val consumablesByCoordinates = mutable.Map[(Int, Int), Circle]()
    def drawConsumables(maze: immutable.Map[(Int,Int), DrawableElement]) {

      def removeConsumable(coordinates: (Int, Int)) {
        // (new AudioClip("file:sounds/waka-waka.mp3")).play()
        root.getChildren.remove(consumablesByCoordinates(coordinates))
        consumablesByCoordinates -= coordinates
      }

      def addConsumable(coordinates: (Int, Int), consumable: DrawableElement) {
        consumablesByCoordinates(coordinates) = new Circle()
        consumablesByCoordinates(coordinates).setFill(PelletColor)
        consumablesByCoordinates(coordinates).setCenterX((coordinates._1 + 0.5) * tileSize)
        consumablesByCoordinates(coordinates).setCenterY((coordinates._2 + 0.5) * tileSize)
        consumable match {
          case _: Dot => consumablesByCoordinates(coordinates).setRadius(3)
          case _: PowerPellet => consumablesByCoordinates(coordinates).setRadius(5)
        }
        root.getChildren.add(consumablesByCoordinates(coordinates))
      }

      maze.foreach {
        case ((coordinates: (Int, Int), elem: DrawableElement)) => 
          elem match {
            case _: Wall => 
            case _: Filled => 
            case space: Space => 
              if (space.consumableAvailable && consumablesByCoordinates.isDefinedAt(coordinates) == false) {
                addConsumable(coordinates, space.consumable)
              } else if (consumablesByCoordinates.isDefinedAt(coordinates) && space.consumableAvailable == false) {
                removeConsumable(coordinates)
              }
          }
      }
    }

	def loop(i: Integer, game: Game) {
		val task = new Task[Unit] {
			override def call(): Unit = {
                if (i == 0) {
                  drawMaze(game.maze)
                  drawConsumables(game.maze)
                  drawMovables(game.movables)
                  (new AudioClip("file:sounds/opening-song.mp3")).play()
                  Thread.sleep(4000) // Wait for clip to play
                  startSiren()
                  game.start()
                } else {
				  Thread.sleep(10) // Wait before drawing again
                }
			}
			override def succeeded() {
				drawMovables(game.movables)
				drawConsumables(game.maze)
                if (game.state == PlayerPlaying) loop(i + 1, game) else handleGameEnd(game.state)
			}
		}
		(new Thread(task, s"frame-{i}")).start()
	}

    def handleGameEnd(gameState: GameState) {
      def animate() {
        def loop(i: Int) {
          val task = new Task[Unit] {
            override def call(): Unit = Thread.sleep(10)
            override def succeeded() {
              mazeRectangles(i).setFill(Color.BLACK)
              loop(i + 1)
            }
          }
          (new Thread(task, s"game-end-{i}")).start()
        }
        loop(0)
      }
      println(s"Game is over. Result: ${ gameState }")
      val text = gameState match {
        case PlayerWon => "You win."
        case PlayerLost => "You lose."
      }
      val textNode = new Text(tilesX/2 * tileSize, tilesY/2 * tileSize, s"${text} \n Thanks for playing!")
      textNode.setFill(Color.WHITE)
      textNode.setTextAlignment(TextAlignment.CENTER)
      root.getChildren.add(textNode)
      (new AudioClip("file:sounds/death.mp3")).play()
      animate()
    }

    def startSiren() {
      val sirenFile = new java.io.File("sounds/siren.mp3")
      println(sirenFile.toURI.toString)
      val clip = new Media(sirenFile.toURI.toString)
      val player = new MediaPlayer(clip)
      def loop(i: Int) {
        val task = new Task[Unit] {
          // REALLY janky way of getting the siren right.
          override def call(): Unit = Thread.sleep(1600)
          override def succeeded() {
            if (game.state == PlayerPlaying) {
              player.seek(new javafx.util.Duration(250))
              player.play()
              loop(i + 1)
            }
          }
        }
        (new Thread(task, s"siren-{i}")).start()
      }
      loop(0)
    }

	loop(0, game)
}

