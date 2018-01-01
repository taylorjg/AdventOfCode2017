const fs = require("fs");

const parseDanceMove = s => {
    switch (s[0]) {
        case "s": return {
            type: "s",
            x: parseInt(s.substr(1), 10)
        };
        case "x": return {
            type: "x",
            a: parseInt(s.substr(1, s.indexOf("/") - 1), 10),
            b: parseInt(s.substr(s.indexOf("/") + 1), 10)
        };
        case "p": return {
            type: "p",
            a: s[1],
            b: s[3]
        };
    }
};

const parseInput = s =>
    s.split(",")
        .filter(line => line.length > 0)
        .map(parseDanceMove);

const makeInitial = n => {
    const a = "a".codePointAt(0);
    const xs = Array.from(Array(n).keys()).map(x => x + a);
    return String.fromCharCode(...xs);
};

const makeSpinMove = (arr, { x }) => {
    const ys = arr.splice(-x, x);
    arr.splice(0, 0, ...ys);
};

const makeExchangeMove = (arr, { a, b }) => {
    const tmp = arr[a];
    arr[a] = arr[b];
    arr[b] = tmp;
};

const makePartnerMove = (arr, { a, b }) => {
    const idxa = arr.indexOf(a);
    const idxb = arr.indexOf(b);
    makeExchangeMove(arr, { a: idxa, b: idxb });
};

const makeMove = (arr, move) => {
    switch (move.type) {
        case "s": makeSpinMove(arr, move); break;
        case "x": makeExchangeMove(arr, move); break;
        case "p": makePartnerMove(arr, move); break;
    }
    return arr;
};

const makeMoves = (arr, moves) => {
    for (move of moves) {
        makeMove(arr, move);
    }
};

const computePart1 = (n, moves) => {
    const initial = makeInitial(n);
    const arr = Array.from(initial);
    makeMoves(arr, moves);
    return arr.join("");
};

const computePart2 = (n, moves, steps) => {
    const initial = makeInitial(n);
    let arr = Array.from(initial);
    for (let step = 0; step < steps; step++) {
        makeMoves(arr, moves);
    }
    return arr.join("");
};

const decompose = moves =>
    [
        moves.filter(m => m.type === "p"),
        moves.filter(m => m.type !== "p")
    ];

const findCycle = (arr, moves) => {
    const s1 = arr.join("");
    for (let n = 0; ; n++) {
        makeMoves(arr, moves);
        const s2 = arr.join("");
        if (s1 === s2) {
            return n;
        }
    }
};

const lcm = (a, b) => {
    for (n = 1; ; n++) {
        if (n % a === 0 && n % b === 0) {
            return n;
        }
    }
};

const computePart2a = (n, moves, steps) => {
    const [ps, nps] = decompose(moves);
    const initial = makeInitial(n);
    const arr = Array.from(initial);
    const a = findCycle(arr, ps);
    const b = findCycle(arr, nps);
    const c = lcm(a, b);
    const d = steps % c;
    return computePart2(n, moves, d);
};

const run = (fileName, label, n, steps) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const moves = parseInput(input);
            console.log(`[${label} input] part1: ${computePart1(n, moves)}`);
            console.log(`[${label} input] part2: ${computePart2a(n, moves, steps)}`);
        }
    });
};
    
const test = () => run("Day16/test/input.txt", "test", 5, 2);
const real = () => run("Day16/src/input.txt", "real", 16, 1000000000);

test();
real();
