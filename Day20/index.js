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

const computePart1 = particles => {
    const TICKS = 10000;
    for (let i = 0; i < TICKS; i++) {
        processParticles(particles);
    }
    return nearestToZero(particles).index;
};

const run = (fileName, label) => {
    fs.readFile(fileName, (err, buffer) => {
        if (err) {
            console.log(`err: ${err}`);
        }
        else {
            const input = buffer.toString();
            const particles = parseInput(input);
            console.log(`[${label} input] ${computePart1(particles)}`);
        }
    });
};

const test = () => run("Day20/test.txt", "test");
const real = () => run("Day20/input.txt", "real");

test();
real();
