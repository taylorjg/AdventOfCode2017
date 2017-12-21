const fs = require("fs");

const parseRows = bit => bit.split("/");

const parseRule = line => {
    const bits = line.split("=>").map(bit => bit.trim());
    return [parseRows(bits[0]), parseRows(bits[1])];
};

const parseInput = input =>
    input.split("\n")
        .filter(line => line.length > 0)
        .map(parseRule);

const rotate90 = grid =>
    grid.map((_, i) =>
        grid.map(line => line[i]).reverse().join(""));

const rotate180 = grid =>
    rotate90(rotate90(grid));

const rotate270 = grid =>
    rotate90(rotate90(rotate90(grid)));

const flipH = grid =>
    grid.map(row => Array.from(row.slice()).reverse().join(""));

const flipV = grid =>
    grid.slice().reverse();

const flatten = xss => [].concat(...xss);

const splitGrid = grid => {
    const size = grid.length;
    const n = (size % 2 === 0) ? 2 : 3;
    const chunkIndices = Array.from(Array(size / n).keys());
    const chunks = chunkIndices.map(i => grid.slice(i * n, (i + 1) * n));
    const v1 = chunks.map(chunk => chunkIndices.map(i => chunk.map(line => line.slice(i * n, (i + 1) * n))));
    const v2 = flatten(v1);
    return v2;
};

const combineGrids = grids => {
    if (grids.length === 1) return grids[0];
    const n = grids[0][0].length;
    const numChunks = Math.sqrt(grids.length);
    console.log(`[dumpGrids] n: ${n}; numChunks: ${numChunks}`);
    const ns = Array.from(Array(n).keys());
    const chunkIndices = Array.from(Array(numChunks).keys());
    const chunks = chunkIndices.map(i => grids.slice(i * numChunks, (i + 1) * numChunks));

    const v1 = chunks.map(chunk => {
        // TODO: remove return
        return ns.map(i => {
            const rows = chunkIndices.map(j => chunk[j][i]);
            const line = rows.join("");
            return line;
        });
    });
    const v2 = flatten(v1);
    return v2;
};

const dumpGrid = grid => {
    grid.forEach(row => console.log(row));
    console.log();
};

const dumpGrids = grids => {
    if (grids.length === 1) return dumpGrid(grids[0]);
    const n = grids[0][0].length;
    const numChunks = Math.sqrt(grids.length);
    console.log(`[dumpGrids] n: ${n}; numChunks: ${numChunks}`);
    const ns = Array.from(Array(n).keys());
    const chunkIndices = Array.from(Array(numChunks).keys());
    const chunks = chunkIndices.map(i => grids.slice(i * numChunks, (i + 1) * numChunks));
    chunks.forEach(chunk => {
        ns.forEach(i => {
            const rows = chunkIndices.map(j => chunk[j][i]);
            const line = rows.join("|");
            console.log(line);
        });
        const dashes = "-".repeat(n);
        const separator = Array(numChunks).fill(dashes).join("+");
        console.log(separator);
    });
    console.log();
};

const countOnPixels = grid =>
    Array.from("".concat(...grid)).filter(ch => ch === "#").length;

const gridsEqual = (g1, g2) => {
    if (g1.length !== g2.length) return false;
    for (let i = 0; i < g1.length; i++) {
        if (g1[i] !== g2[i]) return false;
    }
    return true;
};

const matchRule = (rules, variations) => {
    // TODO: for in / for of ?
    for (let i = 0; i < rules.length; i++) {
        const rule = rules[i];
        // TODO: for in / for of ?
        for (let j = 0; j < variations.length; j++) {
            const variation = variations[j];
            if (gridsEqual(rule[0], variation)) return rule;
        }
    }
};

const enhanceGrid = rules => grid => {
    const variations = [
        grid,
        rotate90(grid),
        rotate180(grid),
        rotate270(grid),
        flipH(grid),
        flipV(grid),
        flipH(rotate90(grid)),
        flipH(rotate180(grid)),
        flipH(rotate270(grid)),
    ];
    const rule = matchRule(rules, variations);
    // TODO: rule.input, rule.output 
    return rule[1];
};

const transformGrid = rules => grid => {
    const v1 = splitGrid(grid);
    const v2 = v1.map(enhanceGrid(rules));
    const v3 = combineGrids(v2);
    return v3;
};

const INITIAL_GRID = [
    ".#.",
    "..#",
    "###"
];

const computePart1 = (rules, numIterations) => {
    const range = Array.from(Array(numIterations).keys());
    const finalGrid = range.reduce(transformGrid(rules), INITIAL_GRID);
    return countOnPixels(finalGrid);
};

const run = (fileName, label, numIterations) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const rules = parseInput(input);
            console.log(`[${label} input] part1: ${computePart1(rules, numIterations)}`);
        }
    });
};

const test = n => run("Day21/test.txt", "test", n);
const real = n => run("Day21/input.txt", "real", n);

const demoRotationsAndFlips = () => {

    const smallGrid = [
        "ABC",
        "DEF",
        "GHI",
    ];

    console.log("rotate90");
    dumpGrid(rotate90(smallGrid));

    console.log("rotate180");
    dumpGrid(rotate180(smallGrid));

    console.log("rotate270");
    dumpGrid(rotate270(smallGrid));

    console.log("flipH");
    dumpGrid(flipH(smallGrid));

    console.log("flipV");
    dumpGrid(flipV(smallGrid));
};

const demoSplitAndCombine = () => {

    const bigGrid1 = [
        "111222333",
        "AAABBBCCC",
        "aaabbbccc",
        "444555666",
        "DDDEEEFFF",
        "dddeeefff",
        "777888999",
        "GGGHHHIII",
        "ggghhhiii"
    ];

    dumpGrid(bigGrid1);

    const grids = splitGrid(bigGrid1);
    dumpGrids(grids);

    const bigGrid2 = combineGrids(grids);
    dumpGrid(bigGrid2);
};

// demoRotationsAndFlips();
// demoSplitAndCombine();

test(2);
real(5);
real(18);