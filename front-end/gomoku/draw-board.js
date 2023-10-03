// npx eslint draw-board.ts
var StoneColor;
(function (StoneColor) {
    StoneColor[StoneColor["Black"] = 0] = "Black";
    StoneColor[StoneColor["White"] = 1] = "White";
})(StoneColor || (StoneColor = {}));
var gameConfig = {
    htmlCanvasName: 'goBoard',
    backgroundPath: 'images/wood_full_original.jpg',
    blackStonePath: 'images/black_00.png',
    whiteStonePath: 'images/white_00.png',
    boardSize: 19,
};
function initGame(cfg) {
    var canvas = document.getElementById(cfg.htmlCanvasName);
    var visibleCtx;
    var visibleCtxMaybe = canvas.getContext('2d');
    if (visibleCtxMaybe !== null) {
        visibleCtx = visibleCtxMaybe;
    }
    else {
        throw new Error("draw-board initGame: visibleCtxMaybe in null");
    }
    var bufferCanvas = document.createElement('canvas');
    bufferCanvas.width = canvas.width;
    bufferCanvas.height = canvas.height;
    var bufferCanvasCtxMaybe = bufferCanvas.getContext('2d');
    var bufferCanvasCtx;
    if (bufferCanvasCtxMaybe !== null) {
        bufferCanvasCtx = bufferCanvasCtxMaybe;
    }
    else {
        throw new Error("draw-board initGame: bufferCanvasCtxMaybe in null");
    }
    var background = new Image();
    background.src = cfg.backgroundPath;
    var blackStone = new Image();
    blackStone.src = cfg.blackStonePath;
    var whiteStone = new Image();
    whiteStone.src = cfg.whiteStonePath;
    // Handle image loading
    background.onload = function () {
        blackStone.onload = function () { };
        whiteStone.onload = function () { };
    };
    var canvases = {
        canvas: canvas,
        visibleCtx: visibleCtx,
        bufferCanvas: bufferCanvas,
        ctx: bufferCanvasCtx,
    };
    var assets = {
        backgroundImg: background,
        blackStoneImg: blackStone,
        whiteStoneImg: whiteStone,
    };
    var sizes = {
        gridSize: canvas.width / (cfg.boardSize + 1),
        borderSize: canvas.width / (cfg.boardSize + 1),
        boardSize: cfg.boardSize,
    };
    var gameState = {
        sizes: sizes,
        mouseX: -1000,
        mouseY: -1000,
        canvases: canvases,
        assets: assets,
        gameLoopId: null,
    };
    document.addEventListener('mousemove', function (event) {
        // Retrieve the mouse cursor coordinates from the event object
        var canvasRect = gameState.canvases.canvas.getBoundingClientRect();
        gameState.mouseX = event.clientX - canvasRect.left; // X-coordinate relative to the viewport
        gameState.mouseY = event.clientY - canvasRect.top; // Y-coordinate relative to the viewport
        // Log or use the cursor coordinates
        console.log("Mouse X: ".concat(gameState.mouseX, ", Mouse Y: ").concat(gameState.mouseY));
    });
    return gameState;
}
// Draw grid lines
function drawGrid(ctx, sizes, canvas) {
    var gridSize = sizes.gridSize, borderSize = sizes.borderSize, boardSize = sizes.boardSize;
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;
    for (var i = 0; i <= sizes.boardSize; i++) {
        var pos = i * gridSize;
        // Vertical lines
        ctx.beginPath();
        ctx.moveTo(pos, borderSize);
        ctx.lineTo(pos, canvas.height - borderSize);
        ctx.stroke();
        // Horizontal lines
        ctx.beginPath();
        ctx.moveTo(borderSize, pos);
        ctx.lineTo(canvas.width - borderSize, pos);
        ctx.stroke();
    }
    // Draw star points (for a 19x19 board)
    if (boardSize === 19) {
        var starPoints_1 = [3 + 1, 9 + 1, 15 + 1];
        ctx.fillStyle = 'black';
        starPoints_1.forEach(function (x) {
            starPoints_1.forEach(function (y) {
                var centerX = x * gridSize;
                var centerY = y * gridSize;
                ctx.beginPath();
                ctx.arc(centerX, centerY, 3, 0, 2 * Math.PI);
                ctx.fill();
            });
        });
    }
}
function placeStone(ctx, assets, sizes, x, y, color) {
    var gridSize = sizes.gridSize, borderSize = sizes.borderSize;
    var coordX = (x - 1) * gridSize - gridSize / 2 + borderSize;
    var coordY = (y - 1) * gridSize - gridSize / 2 + borderSize;
    var image = color == StoneColor.Black ? assets.blackStoneImg : assets.whiteStoneImg;
    ctx.drawImage(image, coordX, coordY, gridSize, gridSize);
}
function drawBackground(ctx, backgroundImg, canvas) {
    ctx.drawImage(backgroundImg, 0, 0, canvas.width, canvas.height);
}
function findNearestToMouseCoord(mouseX, mouseY, canvas, sizes) {
    var gridSize = sizes.gridSize, borderSize = sizes.borderSize, boardSize = sizes.boardSize;
    if (mouseX < 0 || mouseY < 0 || mouseX > canvas.width || mouseY > canvas.height) {
        return null;
    }
    else {
        var nx = Math.round((mouseX - borderSize) / gridSize);
        var ny = Math.round((mouseY - borderSize) / gridSize);
        if (nx >= 0 && ny >= 0 && nx < boardSize && ny < boardSize) {
            var x = nx * gridSize + borderSize - gridSize / 2;
            var y = ny * gridSize + borderSize - gridSize / 2;
            return [x, y];
        }
        else {
            return null;
        }
    }
}
function drawStoneNearMouse(ctx, mouseX, mouseY, canvas, sizes, assets) {
    var maybeNeares = findNearestToMouseCoord(mouseX, mouseY, canvas, sizes);
    if (!maybeNeares) {
        return;
    }
    else {
        var x = maybeNeares[0], y = maybeNeares[1];
        ctx.drawImage(assets.blackStoneImg, x, y, sizes.gridSize, sizes.gridSize);
    }
}
function gameLoop(state) {
    drawBackground(state.canvases.ctx, state.assets.backgroundImg, state.canvases.canvas);
    drawGrid(state.canvases.ctx, state.sizes, state.canvases.canvas);
    placeStone(state.canvases.ctx, state.assets, state.sizes, 10, 10, StoneColor.Black);
    placeStone(state.canvases.ctx, state.assets, state.sizes, 1, 1, StoneColor.White);
    placeStone(state.canvases.ctx, state.assets, state.sizes, 19, 19, StoneColor.White);
    placeStone(state.canvases.ctx, state.assets, state.sizes, 1, 19, StoneColor.White);
    placeStone(state.canvases.ctx, state.assets, state.sizes, 19, 1, StoneColor.White);
    drawStoneNearMouse(state.canvases.ctx, state.mouseX, state.mouseY, state.canvases.canvas, state.sizes, state.assets);
    state.canvases.visibleCtx.drawImage(state.canvases.bufferCanvas, 0, 0);
}
function startGameLoop(state) {
    if (!state.gameLoopId) {
        state.gameLoopId = setInterval(function () {
            gameLoop(state);
        }, 1000 / 60); // Set the desired frame rate (e.g., 60 FPS)
    }
}
var gameState = initGame(gameConfig);
startGameLoop(gameState);
