const fs = require("fs");

const flatten = xss => [].concat(...xss);

const parseLine = (line, y) =>
    Array.from(line)
        .map((ch, x) => ({ ch, x }))
        .filter(({ ch }) => ch === "#")
        .map(({ x }) => ({ x, y }));

const parseInput = input => {
    const lines = input.split("\n").filter(line => line.length > 0);
    const n = (lines.length - 1) / 2;
    const v1 = lines.map(parseLine);
    const v2 = flatten(v1);
    const v3 = v2.map(({ x, y }) => ({ x: x - n, y: n - y }));
    return v3;
}

const equalCoords = c1 => c2 =>
    c1.x === c2.x && c1.y === c2.y;

const calculateNewDirection = (oldDir, infected) => {
    switch (oldDir) {
        case "U": return infected ? "R" : "L";
        case "D": return infected ? "L" : "R";
        case "L": return infected ? "U" : "D";
        case "R": return infected ? "D" : "U";
    }
};

const calculateNewPosition = ({ x, y }, dir) => {
    switch (dir) {
        case "U": return { x, y: y + 1 };
        case "D": return { x, y: y - 1 };
        case "L": return { x: x - 1, y };
        case "R": return { x: x + 1, y };
    }
};

const removeFromMap = (map, pos) => {
    const index = map.findIndex(equalCoords(pos));
    if (index >= 0) {
        map.splice(index, 1);
    }
    return map;
};

const addToMap = (map, pos) => {
    const newMap = map.slice();
    newMap.push(pos);
    return newMap;
};

const reducer = state => {
    const oldInfected = !!state.map.find(equalCoords(state.pos));
    const newInfected = !oldInfected;
    const newDir = calculateNewDirection(state.dir, oldInfected);
    const newPos = calculateNewPosition(state.pos, newDir);
    const newMap = (newInfected) ? addToMap(state.map, state.pos) : removeFromMap(state.map, state.pos)
    return {
        map: newMap,
        pos: newPos,
        dir: newDir,
        numInfectingBursts: state.numInfectingBursts + (newInfected ? 1 : 0)
    };
};

const computePart1 = (map, numIterations) => {
    const initialState = {
        map,
        pos: { x: 0, y: 0 },
        dir: "U",
        numInfectingBursts: 0
    };
    const range = Array.from(Array(numIterations).keys());
    const finalState = range.reduce(reducer, initialState);
    return finalState.numInfectingBursts;
};

const run = (fileName, label, numIterations) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const map = parseInput(input);
            const answer1 = computePart1(map, numIterations);
            console.log(`[${label} input (${numIterations} iterations)] answer1: ${answer1}`);
        }
    });
};

const test = numIterations => run("Day22/test.txt", "test", numIterations);
const real = numIterations => run("Day22/input.txt", "real", numIterations);

test(7);
test(70);
test(10000);
real(10000);
