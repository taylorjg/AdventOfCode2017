const fs = require("fs");

const move = ({ x, y }, step) => {
    switch (step) {
        case "n": return { x, y: y + 1 };
        case "s": return { x, y: y - 1 };
        case "se": return { x: x + 1, y: y - 0.5 };
        case "sw": return { x: x - 1, y: y - 0.5 };
        case "ne": return { x: x + 1, y: y + 0.5 };
        case "nw": return { x: x - 1, y: y + 0.5 };
    };
    throw new Error(`Invalid step, ${step}`);
};

const moves = (start, steps) =>
    steps.reduce(move, start);

const shortestRoute = finish => {
    const dx = Math.abs(finish.x);
    const dy = Math.abs(finish.y);
    if (dx > dy) {
        return (dy * 2) + (dx - dy * 2);
    }
    else {
        const straight = dy - (dx / 2);
        return straight + dx;
    }
};

const showAnswers = stepsString => {

    const steps = stepsString.split(",");
    const start = { x: 0, y: 0 };
    const finish = moves(start, steps);

    console.log(`shortest route is ${shortestRoute(finish)}`);

    const seed = {
        currentLocation: start,
        furthestDistance: 0
    };
    const finalAcc = steps.reduce(
        (acc, step) => {
            const newCurrentLocation = move(acc.currentLocation, step);
            const distance = shortestRoute(newCurrentLocation);
            const newFurthestDistance = Math.max(acc.furthestDistance, distance);
            return {
                currentLocation: newCurrentLocation,
                furthestDistance: newFurthestDistance
            };
        },
        seed);
    console.log(`furthest distance is ${finalAcc.furthestDistance}`);
};

fs.readFile("Day11/src/input.txt", (err, input) => {
    if (err) {
        console.log(`err: ${err}`);
    }
    else {
        showAnswers(input.toString().trim());
    }
});
