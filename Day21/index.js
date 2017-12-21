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

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const rules = parseInput(input);
            console.log(JSON.stringify(rules));
        }
    });
};

const test = () => run("Day21/test.txt", "test");
const real = () => run("Day21/input.txt", "real");

test();
real();
