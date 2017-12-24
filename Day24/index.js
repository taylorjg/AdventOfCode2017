const fs = require("fs");

const parseComponent = line => {
    const bits = line.split("/");
    return {
        a: parseInt(bits[0], 10),
        b: parseInt(bits[1], 10)
    };
};

const parseInput = input =>
    input
        .split("\n")
        .filter(line => line.length > 0)
        .map(parseComponent);

const canConnect = (endUsed, c1, c2) =>
    endUsed === 'a'
        ? (c1.b === c2.a || c1.b === c2.b)
        : (c1.a === c2.a || c1.a === c2.b);

const whichEndUsed = (endUsed, c1, c2) =>
    endUsed === 'a'
        ? (c1.b === c2.a ? 'a' : 'b')
        : (c1.a === c2.a ? 'a' : 'b');

const bridgeStrength = components =>
    components.reduce((sum, c) => sum + c.a + c.b, 0);

const findMax = xs =>
    xs.reduce((a, b) => Math.max(a, b));

const validBridges = components => {

    const nodes = [{
        bridge: [{ a: 0, b: 0 }],
        endUsed: 'a',
        rest: components
    }];

    for (; ;) {
        const newNodes = [];
        const condemnedNodes = [];
        for (node of nodes) {
            const last = node.bridge.slice(-1)[0];
            const matches = node.rest.filter(c => canConnect(node.endUsed, last, c));
            if (matches.length > 0) {
                condemnedNodes.push(node);
                matches.forEach(match => {
                    const clonedBridge = node.bridge.slice();
                    clonedBridge.push(match);
                    newNodes.push({
                        bridge: clonedBridge,
                        endUsed: whichEndUsed(node.endUsed, last, match),
                        rest: node.rest.filter(c => c !== match)
                    })
                });
            }
        }
        if (newNodes.length === 0) break;
        condemnedNodes.forEach(node => {
            const index = nodes.indexOf(node);
            nodes.splice(index, 1);
        });
        nodes.push(...newNodes);
    }

    return nodes.map(node => node.bridge);
};

const computePart1 = bridges =>
    findMax(bridges.map(bridgeStrength));

const computePart2 = bridges => {
    const maxLength = findMax(bridges.map(bridge => bridge.length));
    const longestBridges = bridges.filter(bridge => bridge.length === maxLength);
    return findMax(longestBridges.map(bridgeStrength));
};

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const components = parseInput(input);
            const bridges = validBridges(components);
            console.log(`[${label} input] part1: ${computePart1(bridges)}`);
            console.log(`[${label} input] part2: ${computePart2(bridges)}`);
        }
    });
};

const test = () => run("Day24/test.txt", "test");
const real = () => run("Day24/input.txt", "real");

test();
real();
