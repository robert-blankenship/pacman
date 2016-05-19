# A simple game.
class DrawableElement
class Space extends DrawableElement
    constructor: (@consumableAvailable) ->
class Wall extends DrawableElement

class Tile
    constructor: (@row, @column, @skin) ->

class Movable
    constructor: (movableData) ->
        @x = movableData.x
        @y = movableData.y

getTileHeight = (maze, ctx) ->
    tilesY =
        maze.map (tile) -> tile.row
            .reduce (rowCurrent, rowNext) ->
                if rowNext > rowCurrent then rowNext else rowCurrent
            , 0
    ctx.canvas.height / (tilesY + 1)

getTileWidth = (maze, ctx) ->
    tilesX =
        maze.map (tile) -> tile.column
            .reduce (columnCurrent, columnNext) ->
                if columnNext > columnCurrent then columnNext else columnCurrent
            , 0
    ctx.canvas.width / (tilesX + 1)

drawPellets = (maze, ctx) ->
    tileWidth = getTileWidth(maze, ctx)
    tileHeight = getTileHeight(maze, ctx)

    maze.forEach (tile) ->
        if tile.skin.consumableAvailable
            ctx.beginPath()
            ctx.fillStyle = "yellow"
            ctx.arc(tile.column * tileWidth + tileWidth/2, tile.row * tileHeight + tileHeight/2, 3, 0, 2 * Math.PI)
            ctx.fill()

drawMaze = (maze, ctx) ->
    tileWidth = getTileWidth(maze, ctx)
    tileHeight = getTileHeight(maze, ctx)

    maze.forEach (tile) ->
        ctx.beginPath()
        ctx.fillStyle = 
            if tile.skin instanceof Space
                "blue"
            else if tile.skin instanceof Wall
                "black"
        ctx.fillRect(tile.column * tileWidth, tile.row * tileHeight, tileWidth, tileHeight)
        ctx.stroke()
        
drawMovables = (movables, maze, ctx) ->
    tileWidth = getTileWidth(maze, ctx)

    movables.forEach (movable) ->
        ctx.beginPath()
        ctx.fillStyle = "yellow"
        ctx.arc(tileWidth * movable.x, tileWidth * movable.y, 10, 0, 2 * Math.PI)
        ctx.fill()

class KeyCodes
    @w = 119
    @a = 97
    @s = 115
    @d = 100

    @Up = 38
    @Down = 40
    @Left = 37
    @Right = 39

init = ->
    canvas = document.getElementById('game-canvas')
    context = canvas.getContext('2d')
    
    socket = new WebSocket "ws://localhost:8000"

    maze = []
    movables = []

    siren =  new Audio '../sounds/siren-loop.ogg'
    openingSong = new Audio '../sounds/opening-song.mp3'
    openingSong.play()
    openingSong.onended = ->
        siren.loop = true
        siren.play()

    defeatSong = new Audio '../sounds/death.mp3'
    victorySong = new Audio '../sounds/intermission.mp3'

    window.addEventListener "keydown", (ev) ->
        console.log "player pressed #{ev.keyCode}"

        socket.send switch ev.keyCode
            when KeyCodes.w     then "North"
            when KeyCodes.Up    then "North"
            when KeyCodes.a     then "West"
            when KeyCodes.Left  then "West"
            when KeyCodes.s     then "South"
            when KeyCodes.Down  then "South"
            when KeyCodes.d     then "East"
            when KeyCodes.Right then "East"

    socket.onmessage = (message) ->
        json = JSON.parse message.data

        unless json.maze is undefined
            maze = json.maze.map (tile) ->
                if tile.skin is 'space'
                    new Tile(tile.row, tile.column, new Space(tile.consumableAvailable))
                else if tile.skin is 'wall'
                    new Tile(tile.row, tile.column, new Wall())

        unless json.movables is undefined
            movables = json.movables.map (movableData) -> new Movable(movableData)

        unless json.state is undefined
            switch json.state
                when "player-playing"
                    null
                when "player-won"
                    socket.close()
                    siren.pause()
                    victorySong.play()
                when "player-lost"
                    socket.close()
                    siren.pause()
                    defeatSong.play()

        context.clearRect(0, 0, canvas.width, canvas.height)

        drawMaze(maze, context)
        drawPellets(maze, context)
        drawMovables(movables, maze, context)

init()
