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

const computePart1 = m => {
    const maxClock = Math.max(...Array.from(m.keys()));
    const ticks = Array.from(Array(maxClock + 1).keys());
    const seed = {
        positions: new Map(Array.from(m.keys()).map(k => [k, { pos: 0, dir: true }])),
        catches: []
    };
    const op = (acc, tick) => {
        const value = acc.positions.get(tick);
        if (value) {
            const { pos } = value;
            if (pos === 0) {
                acc.catches.push({ depth: m.get(tick), range: tick });
            }
        }

        for (kvp of acc.positions.entries()) {
            const [range, { pos, dir }] = kvp;
            const depth = m.get(range);
            if (dir) {
                // going down
                const changedDir = pos === depth - 1;
                const newPos = changedDir ? pos - 1 : pos + 1;
                const newDir = changedDir ? !dir : dir;
                const newValue = { pos: newPos, dir: newDir };
                acc.positions.set(range, newValue);
            }
            else {
                // going up
                const changedDir = pos === 0;
                const newPos = changedDir ? pos + 1 : pos - 1;
                const newDir = changedDir ? !dir : dir;
                const newValue = { pos: newPos, dir: newDir };
                acc.positions.set(range, newValue);
            }
        }
        return acc;
    };
    const v1 = ticks.reduce(op, seed);
    const v2 = v1.catches.reduce((acc, { depth, range }) => acc + depth * range, 0);
    return v2;
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
    // console.log(`[test input] part2: ${computePart2(map)}`);
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
            // console.log(`[real input] part2: ${computePart2(map)}`);
        }
    });
};

test();
real();
