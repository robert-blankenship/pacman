
class DrawableElement
class Space extends DrawableElement
class Wall extends DrawableElement

class Tile
    constructor: (@row, @column, @skin) ->

getTileHeight = (maze, ctx) ->
    tilesY =
        maze.map (tile)-> tile.row
            .reduce (rowCurrent, rowNext) ->
                console.log(rowCurrent, rowNext)
                if rowNext > rowCurrent then rowNext else rowCurrent

    ctx.canvas.height / tilesY

getTileWidth = (maze, ctx) ->
    tilesX =
        maze.map (tile)-> tile.column
            .reduce (columnCurrent, columnNext) ->
                if columnNext > columnCurrent then columnNext else columnCurrent

    ctx.canvas.width / tilesX


drawMaze = (maze, ctx) ->
    tileWidth = getTileWidth(maze, ctx)
    tileHeight = getTileHeight(maze, ctx)

    console.log tileWidth
    console.log tileHeight

    maze.forEach (tile) ->
        ctx.fillStyle = 
            if tile.skin instanceof Space
                "blue"
            else if tile.skin instanceof Wall
                "black"
        ctx.fillRect(tile.row * tileWidth, tile.column * tileHeight, tileWidth, tileHeight)

    ctx.stroke()

init = ->

    testMaze = [
        new Tile(0, 0, new Space())
        new Tile(0, 1, new Space())
        new Tile(0, 2, new Space())
        new Tile(0, 3, new Space())
        new Tile(1, 0, new Space())
        new Tile(1, 1, new Space())
        new Tile(1, 2, new Space())
        new Tile(1, 3, new Space())
        new Tile(2, 0, new Wall())
        new Tile(2, 1, new Wall())
        new Tile(2, 2, new Wall())
        new Tile(2, 3, new Wall())
        new Tile(3, 0, new Wall())
        new Tile(3, 1, new Wall())
        new Tile(3, 2, new Wall())
        new Tile(3, 3, new Wall())
    ]

    maze = testMaze

    canvas = document.getElementById('game-canvas')
    context = canvas.getContext('2d')

    drawMaze(maze, context)

init()
