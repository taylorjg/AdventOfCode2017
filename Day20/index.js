const fs = require("fs");

const parseVector = s => {
    const pos1 = s.indexOf("<");
    const pos2 = s.indexOf(">");
    const nums = s.substring(pos1 + 1, pos2)
        .split(",")
        .map(num => num.trim())
        .map(s => parseInt(s, 10));;
    return {
        x: nums[0],
        y: nums[1],
        z: nums[2]
    };
};

const parseParticle = line => {
    const bits = line.split(", ").map(bit => bit.trim());
    return {
        p: parseVector(bits[0]),
        v: parseVector(bits[1]),
        a: parseVector(bits[2])
    };
};

const parseInput = input => {
    const lines = input
        .split("\n")
        .filter(line => line.length > 0);
    return lines.map(parseParticle);
};

const processParticle = particle => {
    particle.v.x += particle.a.x;
    particle.v.y += particle.a.y;
    particle.v.z += particle.a.z;
    particle.p.x += particle.v.x;
    particle.p.y += particle.v.y;
    particle.p.z += particle.v.z;
};

const processParticles = particles =>
    particles.forEach(p => processParticle(p));

const manhattanDistance = particle =>
    Math.abs(particle.p.x) + Math.abs(particle.p.y) + Math.abs(particle.p.z);

const nearestToZero = particles => {
    let result = null;
    particles.forEach((p, index) => {
        const distance = manhattanDistance(p);
        if (!result || distance < result.distance) {
            result = { distance, index };
        }
    });
    return result;
};

const positionsMatch = (p1, p2) =>
    p1.x === p2.x && p1.y === p2.y && p1.z === p2.z;

const findCollisions = particles => {
    const collisions = [];
    particles.map(particle => {
        const index = collisions.findIndex(collision => positionsMatch(collision[0], particle.p));
        if (index >= 0) {
            collisions[index][1] += 1;
        }
        else {
            collisions.push([particle.p, 1]);
        }
    });

    return collisions
        .filter(collision => collision[1] > 1)
        .map(collision => collision[0]);
};

const computePart1 = particles => {

    const ntzs = [];
    let count = 0;

    const CONVERGENCE_THRESHOLD = 300;

    const converged = () => {
        const len = ntzs.length;
        if (len < CONVERGENCE_THRESHOLD) return false;
        if (len > CONVERGENCE_THRESHOLD) ntzs.splice(0, len - CONVERGENCE_THRESHOLD);
        const indices = ntzs.map(ntz => ntz.index);
        const distinctIndices = new Set(indices);
        return distinctIndices.size === 1;
    };

    for (; !converged(); count++) {
        processParticles(particles);
        const ntz = nearestToZero(particles);
        ntzs.push(ntz);
    }

    console.log(`Converged after ${count} ticks`);
    return ntzs[0].index;
};

const computePart2 = particles => {

    for (let count = 0; count < 1000; count++) {
        processParticles(particles);
        const collisions = findCollisions(particles);
        collisions.forEach((collision, index) => {
            particles = particles.filter(particle => !positionsMatch(particle.p, collision));
        });
    }

    return particles.length;
};

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const particles1 = parseInput(input);
            const particles2 = parseInput(input);
            console.log(`[${label} input] part1: ${computePart1(particles1)}`);
            console.log(`[${label} input] part2: ${computePart2(particles2)}`);
        }
    });
};

const test1 = () => run("Day20/test1.txt", "test");
const test2 = () => run("Day20/test2.txt", "test");
const real = () => run("Day20/input.txt", "real");

test1();
test2();
real();
