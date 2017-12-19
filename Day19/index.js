const fs = require("fs");

const parseRoutingDiagram = input => {
    const lines = input
        .split("\n")
        .filter(line => line.length > 0);
    return lines;
};

const computePart1 = rd => {

    let result = "";

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

    const findNextPos = pos => {
        const pos2 = nextPosInDir(pos);
        const ch = lookupPos(pos);
        if (isLetter(ch)) {
            // Maintain direction and collect a letter.
            result += ch;
            return pos2;
        }
        if (ch === '|' || ch === '-') {
            // Maintain direction.
            return pos2;
        }
        if (ch === '+') {
            // Change direction.
            switch (pos2.dir) {
                case 'L':
                case 'R':
                    {
                        const pos3 = nextPosInDir(pos, 'U');
                        const pos4 = nextPosInDir(pos, 'D');
                        const ch3 = lookupPos(pos3);
                        const ch4 = lookupPos(pos4);
                        if (ch3 !== ' ') return pos3;
                        if (ch4 !== ' ') return pos4;
                    }
                case 'U':
                case 'D':
                    {
                        const pos3 = nextPosInDir(pos, 'L');
                        const pos4 = nextPosInDir(pos, 'R');
                        const ch3 = lookupPos(pos3);
                        const ch4 = lookupPos(pos4);
                        if (ch3 !== ' ') return pos3;
                        if (ch4 !== ' ') return pos4;
                    }
            }
        }
        return null;
    };

    const isLetter = ch => ch >= 'A' && ch <= 'Z';

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

    let pos = findInitialPos();

    for (; ;) {
        const pos2 = findNextPos(pos);
        if (!pos2) {
            return result;
        }
        pos = pos2;
    }
};

const test = () => {
    fs.readFile("Day19/test.txt", (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const rd = parseRoutingDiagram(input);
            console.log(`[real input] part1: ${computePart1(rd)}`);
        }
    });
};

const real = () => {
    fs.readFile("Day19/input.txt", (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const rd = parseRoutingDiagram(input);
            console.log(`[real input] part1: ${computePart1(rd)}`);
        }
    });
};

test();
real();
