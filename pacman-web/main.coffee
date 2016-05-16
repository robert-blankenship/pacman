
class DrawableElement
class Space extends DrawableElement
    constructor: (@consumableAvailable) ->
class Wall extends DrawableElement

class Tile
    constructor: (@row, @column, @skin) ->

getTileHeight = (maze, ctx) ->
    tilesY =
        1 + maze.map (tile)-> tile.row
            .reduce (rowCurrent, rowNext) ->
                console.log(rowCurrent, rowNext)
                if rowNext > rowCurrent then rowNext else rowCurrent

    ctx.canvas.height / tilesY

getTileWidth = (maze, ctx) ->
    tilesX =
        1 + maze.map (tile)-> tile.column
            .reduce (columnCurrent, columnNext) ->
                if columnNext > columnCurrent then columnNext else columnCurrent

    ctx.canvas.width / tilesX


drawPellet = (tile, maze, ctx) ->
    tileWidth = getTileWidth(maze, ctx)
    tileHeight = getTileHeight(maze, ctx)
    
    ctx.fillStyle = "yellow"
    ctx.arc(tile.column * tileWidth + tileWidth/2, tile.row * tileHeight + tileHeight/2, 5, 2 * Math.PI)

drawMaze = (maze, ctx) ->
    tileWidth = getTileWidth(maze, ctx)
    tileHeight = getTileHeight(maze, ctx)

    maze.forEach (tile) ->
        ctx.fillStyle = 
            if tile.skin instanceof Space
                if tile.consumableAvailable then drawPellet tile, maze, ctx
                "blue"
            else if tile.skin instanceof Wall
                "black"
        ctx.fillRect(tile.column * tileWidth, tile.row * tileHeight, tileWidth, tileHeight)

    ctx.stroke()



init = ->
    canvas = document.getElementById('game-canvas')
    context = canvas.getContext('2d')

    socket = new WebSocket "ws://localhost:8000"
    socket.onmessage = (message) ->
        json = JSON.parse message.data

        console.log(json)

        unless json.maze is undefined
            maze = json.maze.map (tile) ->
                if tile.skin is 'space'
                    new Tile(tile.row, tile.column, new Space(tile.consumableAvailable))
                else if tile.skin is 'wall'
                    new Tile(tile.row, tile.column, new Wall())




        drawMaze(maze, context)

init()
