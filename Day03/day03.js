// 17  16  15  14  13
// 18              12
// 19              11
// 20              10
// 21  22  23  24  25

const xoffset = (len, i) => {
    const q = Math.floor(i / len);
    const r = i % len;    
    switch (q) {
        case 0: return 0;
        case 1: return -(r + 1);
        case 2: return -len;
        case 3: return -(len - r - 1);
    }
};

const yoffset = (len, i) => {
    const q = Math.floor(i / len);
    const r = i % len;    
    switch (q) {
        case 0: return r + 1;
        case 1: return len;
        case 2: return len - r - 1;
        case 3: return 0;
    }
};

const isOdd = n => n % 2 === 1;

const calcCoords = n => {
    if (n === 1) {
        return [0, 0];
    }
    const sqrt = Math.ceil(Math.sqrt(n));
    const outerSquareSize = isOdd(sqrt) ? sqrt : sqrt + 1;
    const innerSquareSize = outerSquareSize - 2;
    const len = outerSquareSize - 1;
    const i = n - (innerSquareSize * innerSquareSize) - 1;
    return [
        len / 2 + xoffset(len, i),
        -len / 2 + yoffset(len, i)
    ];
};

const manhattanDistance = coords =>
    Math.abs(coords[0]) + Math.abs(coords[1]);

const coords = calcCoords(289326)
console.log(`coords: ${JSON.stringify(coords)}`);

const distance = manhattanDistance(coords);
console.log(`Manhattan distance: ${distance}`);
