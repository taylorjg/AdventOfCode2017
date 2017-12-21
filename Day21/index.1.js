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

const splitGrid = (grid, n) => {
    const topRows = grid.slice(0, n);
    const bottomRows = grid.slice(n);
    const gnw = topRows.map(row => row.slice(0, n));
    const gne = topRows.map(row => row.slice(n));
    const gsw = bottomRows.map(row => row.slice(0, n));
    const gse = bottomRows.map(row => row.slice(n));
    return [gnw, gne, gsw, gse];
};

const combineGrids = (gnw, gne, gsw, gse) => {
    const topRows = gnw.map((row, index) => row + gne[index]);
    const bottomRows = gsw.map((row, index) => row + gse[index]);
    return [...topRows, ...bottomRows];
};

const dumpGrid = grid => {
    grid.forEach(row => console.log(row));
    console.log();
};

const dumpGrids = (gnw, gne, gsw, gse) => {
    gnw.map((row, index) => console.log(`${row}|${gne[index]}`));
    const dashes = "-".repeat(gnw[0].length);
    console.log(`${dashes}+${dashes}`);
    gsw.map((row, index) => console.log(`${row}|${gse[index]}`));
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
        "111222",
        "111222",
        "111222",
        "333444",
        "333444",
        "333444"
    ];
    
    dumpGrid(bigGrid1);
    
    const grids = splitGrid(bigGrid1, bigGrid1[0].length / 2);
    dumpGrids(...grids);
    
    const bigGrid2 = combineGrids(...grids);
    dumpGrid(bigGrid2);
};

// demoRotationsAndFlips();
// demoSplitAndCombine();

test();
// real();
