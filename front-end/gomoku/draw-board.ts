// npx eslint draw-board.ts

enum StoneColor {
    Black,
    White,
}

interface GameConfig {
    htmlCanvasName: string,
    backgroundPath: string,
    blackStonePath: string,
    whiteStonePath: string,
    boardSize: number,
}

const gameConfig: GameConfig = {
    htmlCanvasName: 'goBoard',
    backgroundPath: 'images/wood_full_original.jpg',
    blackStonePath: 'images/black_00.png',
    whiteStonePath: 'images/white_00.png',
    boardSize: 19,
}

interface GameAssets {
    backgroundImg: HTMLImageElement,
    blackStoneImg: HTMLImageElement,
    whiteStoneImg: HTMLImageElement,
}

interface Canvases {
    canvas: HTMLCanvasElement;
    visibleCtx: CanvasRenderingContext2D;
    bufferCanvas: HTMLCanvasElement;
    ctx: CanvasRenderingContext2D;
}

interface Sizes {
    gridSize: number,
    borderSize: number,
    boardSize: number,
}

interface GameState {
    sizes: Sizes,
    mouseX: number,
    mouseY: number,
    canvases: Canvases,
    assets: GameAssets,
    gameLoopId: number | null,
}

function initGame(cfg: GameConfig): GameState {

    const canvas = document.getElementById(cfg.htmlCanvasName) as HTMLCanvasElement;
    let visibleCtx: CanvasRenderingContext2D;
    const visibleCtxMaybe = canvas.getContext('2d');
    if (visibleCtxMaybe !== null) {
        visibleCtx = visibleCtxMaybe;
    } else {
        throw new Error("draw-board initGame: visibleCtxMaybe in null");
    }



    const bufferCanvas = document.createElement('canvas');
    bufferCanvas.width = canvas.width;
    bufferCanvas.height = canvas.height;

    const bufferCanvasCtxMaybe = bufferCanvas.getContext('2d');
    let bufferCanvasCtx: CanvasRenderingContext2D;
    if (bufferCanvasCtxMaybe !== null) {
        bufferCanvasCtx = bufferCanvasCtxMaybe;
    } else {
        throw new Error("draw-board initGame: bufferCanvasCtxMaybe in null");
    }

    const background = new Image();
    background.src = cfg.backgroundPath;

    const blackStone = new Image();
    blackStone.src = cfg.blackStonePath;

    const whiteStone = new Image();
    whiteStone.src = cfg.whiteStonePath;

    // Handle image loading
    background.onload = () => {
        blackStone.onload = () => { }
        whiteStone.onload = () => { }
    };


    const canvases: Canvases = {
        canvas: canvas,
        visibleCtx: visibleCtx,
        bufferCanvas: bufferCanvas,
        ctx: bufferCanvasCtx,
    }

    const assets: GameAssets = {
        backgroundImg: background,
        blackStoneImg: blackStone,
        whiteStoneImg: whiteStone,
    }

    const sizes: Sizes = {
        gridSize: canvas.width / (cfg.boardSize + 1),
        borderSize: canvas.width / (cfg.boardSize + 1),
        boardSize: cfg.boardSize,
    }

    const gameState: GameState = {
        sizes: sizes,
        mouseX: -1000,
        mouseY: -1000,
        canvases: canvases,
        assets: assets,
        gameLoopId: null,
    }

    document.addEventListener('mousemove', (event: MouseEvent) => {
        // Retrieve the mouse cursor coordinates from the event object
        const canvasRect = gameState.canvases.canvas.getBoundingClientRect();
        gameState.mouseX = event.clientX - canvasRect.left; // X-coordinate relative to the viewport
        gameState.mouseY = event.clientY - canvasRect.top; // Y-coordinate relative to the viewport

        // Log or use the cursor coordinates
        console.log(`Mouse X: ${gameState.mouseX}, Mouse Y: ${gameState.mouseY}`);
    });

    return gameState
}


// Draw grid lines
function drawGrid(ctx: CanvasRenderingContext2D, sizes: Sizes, canvas: HTMLCanvasElement) {
    const { gridSize, borderSize, boardSize } = sizes

    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;

    for (let i = 0; i <= sizes.boardSize; i++) {
        const pos = i * gridSize;

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
        const starPoints = [3 + 1, 9 + 1, 15 + 1];
        ctx.fillStyle = 'black';

        starPoints.forEach(x => {
            starPoints.forEach(y => {
                const centerX = x * gridSize;
                const centerY = y * gridSize;
                ctx.beginPath();
                ctx.arc(centerX, centerY, 3, 0, 2 * Math.PI);
                ctx.fill();
            });
        });
    }
}

function placeStone(ctx: CanvasRenderingContext2D, assets: GameAssets, sizes: Sizes, x: number, y: number, color: StoneColor) {
    const { gridSize, borderSize } = sizes

    const coordX = (x - 1) * gridSize - gridSize / 2 + borderSize;
    const coordY = (y - 1) * gridSize - gridSize / 2 + borderSize;

    const image = color == StoneColor.Black ? assets.blackStoneImg : assets.whiteStoneImg
    ctx.drawImage(image, coordX, coordY, gridSize, gridSize);
}


function drawBackground(ctx: CanvasRenderingContext2D, backgroundImg: HTMLImageElement, canvas: HTMLCanvasElement) {
    ctx.drawImage(backgroundImg, 0, 0, canvas.width, canvas.height);
}


function findNearestToMouseCoord(mouseX: number, mouseY: number, canvas: HTMLCanvasElement, sizes: Sizes): [number, number] | null {
    const { gridSize, borderSize, boardSize } = sizes

    if (mouseX < 0 || mouseY < 0 || mouseX > canvas.width || mouseY > canvas.height) {
        return null
    } else {
        const nx = Math.round((mouseX - borderSize) / gridSize);
        const ny = Math.round((mouseY - borderSize) / gridSize);
        if (nx >= 0 && ny >= 0 && nx < boardSize && ny < boardSize) {
            const x = nx * gridSize + borderSize - gridSize / 2;
            const y = ny * gridSize + borderSize - gridSize / 2;
            return [x, y];
        } else {
            return null;
        }
    }
}

function drawStoneNearMouse(ctx: CanvasRenderingContext2D, mouseX: number, mouseY: number, canvas: HTMLCanvasElement, sizes: Sizes, assets: GameAssets) {
    const maybeNeares = findNearestToMouseCoord(mouseX, mouseY, canvas, sizes);
    if (!maybeNeares) {
        return
    } else {
        const [x, y] = maybeNeares;
        ctx.drawImage(assets.blackStoneImg, x, y, sizes.gridSize, sizes.gridSize);
    }
}

function gameLoop(state: GameState): void {
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

function startGameLoop(state: GameState): void {
    if (!state.gameLoopId) {
        state.gameLoopId = setInterval(() => {
            gameLoop(state);
        }, 1000 / 60); // Set the desired frame rate (e.g., 60 FPS)
    }
}

const gameState = initGame(gameConfig);
startGameLoop(gameState)


