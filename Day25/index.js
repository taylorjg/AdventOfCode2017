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
        states: new Map(states.map(state => [state.name, state]))
    };
};

const runBlueprint = blueprint => {
    const tape = new Map();
    let cursor = 0;
    let currState = "A";
    for (let step = 0; step < blueprint.numSteps; step++) {
        const state = blueprint.states.get(currState);
        const currValue = tape.get(cursor) || 0;
        const rule = (currValue === 1) ? state.rule1 : state.rule0;
        tape.set(cursor, rule.value);
        cursor += rule.direction === "right" ? 1 : -1;
        currState = rule.nextState;
    }
    return tape;
};

const computePart1 = blueprint => {
    const tape = runBlueprint(blueprint);
    const numOnes = Array.from(tape.values()).filter(value => value === 1).length;
    return numOnes;
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
        }
    });
};

const test = () => run("Day25/test/input.txt", "test");
const real = () => run("Day25/src/input.txt", "real");

test();
real();
