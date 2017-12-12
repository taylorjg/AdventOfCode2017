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

const parseInput = lines => lines.map(parseLine);

const loop = (m, k, s1, s2) => {
    s1.add(k);
    s2.add(k);
    vs = m.get(k) || [];
    vs.forEach(v => {
        if (!s2.has(v)) {
            loop(m, v, s1, s2);
        }
    });
};

const discover = m => {
    const sets = [];
    for (k of m.keys()) {
        const s1 = new Set();
        const s2 = new Set();
        loop(m, k, s1, s2);
        sets.push(s1);
    }
    return sets.map(s => Array.from(s.values())).filter(arr => arr.includes(0));
};

const input = `
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
`;

{
    const lines = input.split("\n").filter(line => line.length > 0);
    const map = new Map(parseInput(lines));
    console.log(`arrs.length: ${discover(map).length}`);
}

fs.readFile("Day12/input.txt", (err, buffer) => {
    if (err) {
        console.log(`err: ${err}`);
    }
    else {
        const input = buffer.toString();
        const lines = input.split("\n").filter(line => line.length > 0);
        const map = new Map(parseInput(lines));
        console.log(`arrs.length: ${discover(map).length}`);
    }
});
