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

const calculateNewPosition = ({ x, y }, dir) => {
    switch (dir) {
        case "U": return { x, y: y + 1 };
        case "D": return { x, y: y - 1 };
        case "L": return { x: x - 1, y };
        case "R": return { x: x + 1, y };
    }
};

const mkKey = pos => `${pos.x},${pos.y}`;

const computePart1 = (coords, numIterations) => {

    const calculateNewDirection = (oldDir, infected) => {
        switch (oldDir) {
            case "U": return infected ? "R" : "L";
            case "D": return infected ? "L" : "R";
            case "L": return infected ? "U" : "D";
            case "R": return infected ? "D" : "U";
        }
    };

    const reducer = state => {
        // const oldInfected = !!state.map.find(equalCoords(state.pos));
        const key = mkKey(state.pos);
        const oldInfected = state.map[key] || false;
        const newInfected = !oldInfected;
        const newDir = calculateNewDirection(state.dir, oldInfected);
        const newPos = calculateNewPosition(state.pos, newDir);

        if (newInfected) {
            state.map[key] = true;
        }
        else {
            delete state.map[key];
        }

        return {
            map: state.map,
            pos: newPos,
            dir: newDir,
            numInfectingBursts: state.numInfectingBursts + (newInfected ? 1 : 0)
        };
    };

    const initialState = {
        map: coords.reduce((m, p) => (m[mkKey(p)] = true, m), {}),
        pos: { x: 0, y: 0 },
        dir: "U",
        numInfectingBursts: 0
    };
    const range = Array.from(Array(numIterations).keys());
    const finalState = range.reduce(reducer, initialState);
    return finalState.numInfectingBursts;
};

const computePart2 = (coords, numIterations) => {

    const CLEAN = 0;
    const INFECTED = 1;
    const WEAKENED = 2;
    const FLAGGED = 3;

    const TABLE = {
        "U": {
            [CLEAN]: "L",
            [WEAKENED]: "U",
            [INFECTED]: "R",
            [FLAGGED]: "D"
        },
        "D": {
            [CLEAN]: "R",
            [WEAKENED]: "D",
            [INFECTED]: "L",
            [FLAGGED]: "U"
        },
        "L": {
            [CLEAN]: "D",
            [WEAKENED]: "L",
            [INFECTED]: "U",
            [FLAGGED]: "R"
        },
        "R": {
            [CLEAN]: "U",
            [WEAKENED]: "R",
            [INFECTED]: "D",
            [FLAGGED]: "L"
        }
    };

    const calculateNewDirection = (oldDir, oldState) => TABLE[oldDir][oldState];

    const calculateNewState = oldState => {
        switch (oldState) {
            case CLEAN: return WEAKENED;
            case WEAKENED: return INFECTED;
            case INFECTED: return FLAGGED;
            case FLAGGED: return CLEAN;
        }
    };

    const reducer = (state, iteration) => {
        const key = mkKey(state.pos);
        const oldState = state.map[key] || CLEAN;
        const newState = calculateNewState(oldState);
        const newDir = calculateNewDirection(state.dir, oldState);
        const newPos = calculateNewPosition(state.pos, newDir);

        if (newState === CLEAN) {
            delete state.map[key];
        }
        else {
            state.map[key] = newState;
        }

        return {
            map: state.map,
            pos: newPos,
            dir: newDir,
            numInfectingBursts: state.numInfectingBursts + (newState === INFECTED ? 1 : 0)
        };
    };

    const initialState = {
        map: coords.reduce((m, p) => (m[mkKey(p)] = INFECTED, m), {}),
        pos: { x: 0, y: 0 },
        dir: "U",
        numInfectingBursts: 0
    };
    const range = Array.from(Array(numIterations).keys());
    const finalState = range.reduce(reducer, initialState);
    return finalState.numInfectingBursts;
};

const run = (fileName, label, part, numIterations) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const coords = parseInput(input);
            switch (part) {
                case 1:
                    const answer1 = computePart1(coords, numIterations);
                    console.log(`[${label} input (${numIterations} iterations)] part1: ${answer1}`);
                    break;
                case 2:
                    const answer2 = computePart2(coords, numIterations);
                    console.log(`[${label} input (${numIterations} iterations)] part2: ${answer2}`);
                    break;
            }
        }
    });
};

const test = (part, numIterations) => run("Day22/test.txt", "test", part, numIterations);
const real = (part, numIterations) => run("Day22/input.txt", "real", part, numIterations);

test(1, 7);
test(1, 70);
test(1, 10000);
real(1, 10000);

test(2, 100);
test(2, 10000000);
real(2, 10000000);
