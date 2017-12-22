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
    map.push(pos);
};

const computePart1 = (map, numIterations) => {

    const calculateNewDirection = (oldDir, infected) => {
        switch (oldDir) {
            case "U": return infected ? "R" : "L";
            case "D": return infected ? "L" : "R";
            case "L": return infected ? "U" : "D";
            case "R": return infected ? "D" : "U";
        }
    };

    const reducer = state => {
        const oldInfected = !!state.map.find(equalCoords(state.pos));
        const newInfected = !oldInfected;
        const newDir = calculateNewDirection(state.dir, oldInfected);
        const newPos = calculateNewPosition(state.pos, newDir);

        if (newInfected) {
            addToMap(state.map, state.pos);
        }
        else {
            removeFromMap(state.map, state.pos);
        }

        return {
            map: state.map,
            pos: newPos,
            dir: newDir,
            numInfectingBursts: state.numInfectingBursts + (newInfected ? 1 : 0)
        };
    };

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

const computePart2 = (map, numIterations) => {

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
        if (iteration % 10000 === 0) {
            console.log(`iteration: ${iteration}`);
        }
        const elem = state.map.find(equalCoords(state.pos));;
        const oldState = elem ? elem.state : CLEAN;
        const newState = calculateNewState(oldState);
        const newDir = calculateNewDirection(state.dir, oldState);
        const newPos = calculateNewPosition(state.pos, newDir);
        
        if (newState === CLEAN) {
            removeFromMap(state.map, elem);
        }
        else {
            if (elem) {
                elem.state = newState;
            }
            else {
                const newElem = {
                    x: state.pos.x,
                    y: state.pos.y,
                    state: newState
                };
                addToMap(state.map, newElem);
            }
        }

        return {
            map: state.map,
            pos: newPos,
            dir: newDir,
            numInfectingBursts: state.numInfectingBursts + (newState === INFECTED ? 1 : 0)
        };
    };

    const initialState = {
        map: map.map(({ x, y }) => ({ x, y, state: INFECTED })),
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
            // const map1 = parseInput(input);
            // const answer1 = computePart1(map1, numIterations);
            // console.log(`[${label} input (${numIterations} iterations)] answer1: ${answer1}`);
            const map2 = parseInput(input);
            const answer2 = computePart2(map2, numIterations);
            console.log(`[${label} input (${numIterations} iterations)] answer2: ${answer2}`);
        }
    });
};

const test = numIterations => run("Day22/test.txt", "test", numIterations);
const real = numIterations => run("Day22/input.txt", "real", numIterations);

// test(7);
// test(70);
// test(10000);
// real(10000);
// test(100);
// test(10000000);
real(10000000);
