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

const equalComponents = (c1, c2) =>
    c1.a === c2.a && c1.b === c2.b;

const bridgeStrength = components =>
    components.reduce((sum, component) => sum + component.a + component.b, 0);

const findBestBridge = bridges =>
    bridges.reduce((bestBridge, bridge) => {
        const strength = bridgeStrength(bridge);
        if (strength > bestBridge[1]) {
            return [bridge, strength];
        }
        return bestBridge;
    }, [null, 0]);

const computePart1 = components => {

    const startingPoints = components.filter(component => component.a === 0);
    const nodes = startingPoints.map(sp => ({
        bridge: [sp],
        endUsed: 'a',
        rest: components.filter(c => !equalComponents(c, sp))
    }));

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
                        rest: node.rest.filter(c => !equalComponents(c, match))
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

    const bridges = nodes.map(node => node.bridge);
    const bestBridge = findBestBridge(bridges);
    return bestBridge[1];
};

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const components = parseInput(input);
            console.log(`[${label} input] part1: ${computePart1(components)}`);
            // console.log(`[${label} input] part2: ${computePart2(particles2)}`);
        }
    });
};

const test = () => run("Day24/test.txt", "test");
const real = () => run("Day24/input.txt", "real");

test();
real();
