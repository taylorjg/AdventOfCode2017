const fs = require("fs");

const parseLine = line => {
    const bits = line.split("<->").map(bit => bit.trim());
    const key = parseInt(bits[0], 10);
    const values = bits[1]
        .split(",")
        .map(value => value.trim())
        .map(value => parseInt(value, 10));
    return [key, values];
};

const parseLines = lines => lines.map(parseLine);

const parseString = s => {
    const lines = s
        .split("\n")
        .map(line => line.trim())
        .filter(line => line.length > 0);
    return new Map(parseLines(lines));
};

const loop = (m, k, s1, s2) => {
    s1.add(k);
    s2.add(k);
    vs = m.get(k) || [];
    for (v of vs) {
        if (!s2.has(v)) {
            loop(m, v, s1, s2);
        }
    }
};

const discover = (m, n) => {
    const sets = [];
    for (k of m.keys()) {
        const s1 = new Set();
        const s2 = new Set();
        loop(m, k, s1, s2);
        sets.push(s1);
    }
    return sets.map(s => Array.from(s.values())).filter(arr => arr.includes(n));
};

const computePart1 = m => {
    const arrs = discover(m, 0);
    return arrs.length;
};

const computePart2 = m => {
    const arrs = [];
    for (k of m.keys()) {
        const arr = discover(m, k);
        arrs.push(arr);
    }
    const hashes = arrs.map(arr => arr.reduce((acc, n) => acc + n, 0));
    const distinctHashes = new Set(hashes);
    return distinctHashes.size;
};

const test = () => {
    const input = `
    0 <-> 2
    1 <-> 1
    2 <-> 0, 3, 4
    3 <-> 2, 4
    4 <-> 2, 3, 6
    5 <-> 6
    6 <-> 4, 5
    `;
    const map = parseString(input);
    console.log(`[test input] part1: ${computePart1(map)}`);
    console.log(`[test input] part2: ${computePart2(map)}`);
};

const real = () => {
    fs.readFile("Day12/input.txt", (err, buffer) => {
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
