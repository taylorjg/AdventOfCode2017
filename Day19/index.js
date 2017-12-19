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

    let result = "";
    let stepCount = 0;

    const findNextPos = pos => {
        const nextPos = nextPosInDir(pos);

        // This doesn't look right - but it works!
        // Surely should be lookupPos(pos) ?
        const ch = lookupPos(pos);

        if (isLetter(ch)) {
            // Maintain direction and collect a letter.
            result += ch;
            stepCount++;
            return nextPos;
        }
        if (ch === '|' || ch === '-') {
            // Maintain direction.
            stepCount++;
            return nextPos;
        }
        if (ch === '+') {
            // Change direction.
            switch (nextPos.dir) {
                case 'L':
                case 'R':
                    {
                        const pos3 = nextPosInDir(pos, 'U');
                        const pos4 = nextPosInDir(pos, 'D');
                        const ch3 = lookupPos(pos3);
                        const ch4 = lookupPos(pos4);
                        if (ch3 !== ' ') return (stepCount++, pos3);
                        if (ch4 !== ' ') return (stepCount++, pos4);
                    }
                case 'U':
                case 'D':
                    {
                        const pos3 = nextPosInDir(pos, 'L');
                        const pos4 = nextPosInDir(pos, 'R');
                        const ch3 = lookupPos(pos3);
                        const ch4 = lookupPos(pos4);
                        if (ch3 !== ' ') return (stepCount++, pos3);
                        if (ch4 !== ' ') return (stepCount++, pos4);
                    }
            }
        }

        return null;
    };

    for (let pos = findInitialPos(); !!pos; pos = findNextPos(pos));
    return { result, stepCount };
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
