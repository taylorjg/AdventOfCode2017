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
    const size = grids.length;
    const n = (size % 2 === 0) ? 2 : 3;
    const chunkIndices = Array.from(Array(size / n).keys());
    const chunks = chunkIndices.map(i => grids.slice(i * n, (i + 1) * n));
    const v1 = chunks.map(chunk => {
        return chunkIndices.map(i => {
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
    const size = grids.length;
    const n = (size % 2 === 0) ? 2 : 3;
    const chunkIndices = Array.from(Array(size / n).keys());
    const chunks = chunkIndices.map(i => grids.slice(i * n, (i + 1) * n));
    chunks.forEach(chunk => {
        chunkIndices.forEach(i => {
            const rows = chunkIndices.map(j => chunk[j][i]);
            const line = rows.join("|");
            console.log(line);
        });
        const dashes = "-".repeat(n);
        const separator = Array(n).fill(dashes).join("+");
        console.log(separator);
    });
    console.log();
};

const countOnPixels = grid =>
    Array.from("".concat(...grid)).filter(ch => ch === "#").length;

const computePart1 = rules => {
    // iterate n times
    //   split grid
    return 0;
};

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const rules = parseInput(input);
            console.log(`[${label} input] part1: ${computePart1(rules)}`);
        }
    });
};

const test = () => run("Day21/test.txt", "test");
const real = () => run("Day21/input.txt", "real");

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

// test();
// real();
