const fs = require("fs");

const parseRule = (lines, currLine) => {
    const value = parseInt(/- Write the value (\d+)./.exec(lines[currLine])[1], 10);
    const direction = /- Move one slot to the (\w+)./.exec(lines[currLine + 1])[1];
    const nextState = /- Continue with state (\w+)./.exec(lines[currLine + 2])[1];
    return {
        value,
        direction,
        nextState
    };    
};

const parseState = (lines, currLine) => {
    const name = /In state (\w+):/.exec(lines[currLine])[1];
    const rule0 = parseRule(lines, currLine + 2);
    const rule1 = parseRule(lines, currLine + 6);
    return {
        name,
        rule0,
        rule1
    };
};

const parseBlueprint = input => {

    const lines = input.split("\n");

    const numSteps = parseInt(/Perform a diagnostic checksum after (\d+) steps./.exec(lines[1])[1], 10);

    const states = [];
    let currLine = 3;
    for (;;) {
        if (currLine >= lines.length) break;
        const state = parseState(lines, currLine);
        states.push(state);
        currLine += 10;
    }

    return {
        numSteps,
        states
    };
};

const computePart1 = blueprint => {
    console.log(JSON.stringify(blueprint));
    return 0;
};

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const blueprint = parseBlueprint(input);
            console.log(`[${label} input] part1: ${computePart1(blueprint)}`);
            // console.log(`[${label} input] part2: ${computePart2(bridges)}`);
        }
    });
};

const test = () => run("Day25/test.txt", "test");
const real = () => run("Day25/input.txt", "real");

test();
real();
