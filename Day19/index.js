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

    let letters = "";
    let stepCount = 0;

    const findNextPos = currPos => {

        if (isLetter(lookupPos(currPos))) {
            letters += lookupPos(currPos);
        }

        const nextPos = nextPosInDir(currPos);
        const ch = lookupPos(nextPos);
        stepCount += 1;

        if (ch === '|' || ch === '-' || isLetter(ch)) {
            // Maintain direction.
            return nextPos;
        }

        if (ch === '+') {
            // Change direction.
            const newDirPos = findNewDir(nextPos);
            if (newDirPos) {
                stepCount += 1;
                return newDirPos;
            }
        }

        return null;
    };

    for (let currPos = findInitialPos(); !!currPos; currPos = findNextPos(currPos));
    return { letters, stepCount };
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
real();
