const fs = require("fs");

const parseLine = line => {
    const bits = line.split(":").map(bit => bit.trim());
    const key = parseInt(bits[0], 10);
    const value = parseInt(bits[1], 10);
    return [key, value];
};

const parseLines = lines => lines.map(parseLine);

const parseString = s => {
    const lines = s
        .split("\n")
        .map(line => line.trim())
        .filter(line => line.length > 0);
    return new Map(parseLines(lines));
};

const initialPositions = (m, delay) =>
    new Map(Array.from(m.keys()).map(range => {
        const depth = m.get(range);
        const depthLessOne = depth - 1;
        const q = Math.floor(delay / depthLessOne);
        const r = delay % depthLessOne;
        const isEven = q % 2 === 0;
        const dir = isEven;
        const pos = isEven ? r : depthLessOne - r;
        const scanner = { depth, range, pos, dir };
        return [range, scanner]
    }))

const detectCatches = (acc, range) => {
    const scanner = acc.positions.get(range);
    if (scanner && scanner.pos === 0) {
        acc.catches.push(scanner);
    }
};

const moveScanners = positions => {
    for (kvp of positions.entries()) {
        const [range, { depth, pos, dir }] = kvp;
        if (dir) {
            // scanning down (pos increasing)
            const changedDir = pos === depth - 1;
            const newPos = changedDir ? pos - 1 : pos + 1;
            const newDir = changedDir ? !dir : dir;
            const newScanner = { depth, range, pos: newPos, dir: newDir };
            positions.set(range, newScanner);
        }
        else {
            // scanning up (pos decreasing)
            const changedDir = pos === 0;
            const newPos = changedDir ? pos + 1 : pos - 1;
            const newDir = changedDir ? !dir : dir;
            const newScanner = { depth, range, pos: newPos, dir: newDir,  };
            positions.set(range, newScanner);
        }
    }
};

const crossFirewall = (m, delay) => {
    const seed = {
        positions: initialPositions(m, delay),
        catches: []
    };
    const op = (acc, tick) => {
        detectCatches(acc, tick);
        moveScanners(acc.positions);
        return acc;
    };
    const maxRange = Math.max(...Array.from(m.keys()));
    const ranges = Array.from(Array(maxRange + 1).keys());
    const finalAcc = ranges.reduce(op, seed);
    return finalAcc.catches;
};

const computePart1 = m => {
    const catches = crossFirewall(m, 0);
    return catches.reduce((acc, { depth, range }) => acc + depth * range, 0);
};

const computePart2 = m => {

    function* delays() {
        let delay = 0;
        for(;;) {
            yield delay++;
        }
    }

    for (const it = delays();;) {
        const delay = it.next().value;
        const catches = crossFirewall(m, delay);
        if (catches.length === 0) {
            return delay;
        }
    }
};    

const test = () => {
    const input = `
    0: 3
    1: 2
    4: 4
    6: 4
    `;
    const map = parseString(input);
    console.log(`[test input] part1: ${computePart1(map)}`);
    console.log(`[test input] part2: ${computePart2(map)}`);
};

const real = () => {
    fs.readFile("Day13/input.txt", (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const map = parseString(input);
            console.log(`[real input] part1: ${computePart1(map)}`);
            console.log(`[real input] part2: ${computePart2(map)}`);
        }
    });
};

test();
real();
