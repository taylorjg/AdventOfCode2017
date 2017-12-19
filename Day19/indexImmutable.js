const fs = require("fs");

const parseRoutingDiagram = input => {
    const lines = input
        .split("\n")
        .filter(line => line.length > 0);
    return lines;
};

const computePart1 = rd => {

    const isLetter = ch => ch >= 'A' && ch <= 'Z';

    const lookupPos = ({ x, y }) => {
        if (x < 0 || y < 0 || y >= rd.length || x >= rd[0].length) {
            return null;
        }
        return rd[y][x];
    }

    const findInitialPos = () => {
        const y = 0;
        const x = rd[y].indexOf('|');
        const dir = 'D';
        return { x, y, dir };
    };

    const nextPosInDir = (pos, optionalDir) => {
        const { x, y } = pos;
        const dir = optionalDir || pos.dir;
        switch (dir) {
            case 'L':
                return { x: x - 1, y, dir };
            case 'R':
                return { x: x + 1, y, dir };
            case 'U':
                return { x, y: y - 1, dir };
            case 'D':
                return { x, y: y + 1, dir };
        }
    }

    const findNewDir = pos => {
        const isLR = pos => pos.dir === 'L' || pos.dir === 'R';
        const pos1 = nextPosInDir(pos, isLR(pos) ? 'U' : 'L');
        const pos2 = nextPosInDir(pos, isLR(pos) ? 'D' : 'R');
        if (lookupPos(pos1) !== ' ') return pos1;
        if (lookupPos(pos2) !== ' ') return pos2;
        return null;
    };

    const followPath = state => {

        const { currPos, letters, stepCount } = state;
        const currChar = lookupPos(currPos);
        const newLetters = letters + (isLetter(currChar) ? currChar : "");

        const nextPos = nextPosInDir(currPos);
        const nextChar = lookupPos(nextPos);

        if (nextChar === '|' || nextChar === '-' || isLetter(nextChar)) {
            // Maintain direction.
            return followPath({
                currPos: nextPos,
                letters: newLetters,
                stepCount: stepCount + 1
            });
        }

        if (nextChar === '+') {
            // Change direction.
            const newDirPos = findNewDir(nextPos);
            if (newDirPos) {
                return followPath({
                    currPos: newDirPos,
                    letters: newLetters,
                    stepCount: stepCount + 2
                });
            }
        }

        return {
            currPos,
            letters: newLetters,
            stepCount: stepCount + 1
        };
    };

    const initialState = {
        currPos: findInitialPos(),
        letters: "",
        stepCount: 0
    };

    return followPath(initialState);
};

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const rd = parseRoutingDiagram(input);
            console.log(`[${label} input] ${JSON.stringify(computePart1(rd))}`);
        }
    });
};

const test = () => run("Day19/test.txt", "test");
const real = () => run("Day19/input.txt", "real");

test();

// "Maximum call stack size exceeded" due to lack of TCO.
// We could fix this using a trampoline.
// https://en.wikipedia.org/wiki/Tail_call
// https://en.wikipedia.org/wiki/Trampoline_(computing)
// http://raganwald.com/2013/03/28/trampolines-in-javascript.html
// real();
