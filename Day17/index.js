const spinlock1 = steps => {
    const buffer = [0];
    let pos = 0;
    for (let i = 1; i <= 2017; i++) {
        pos = (pos + steps) % i + 1;
        buffer.splice(pos, 0, i);
    }
    return buffer[pos + 1];
};

const spinlock2 = steps => {
    let pos = 0;
    let zeroPos = 0;
    let result = -1;
    for (let i = 1; i <= 50000000; i++) {
        pos = (pos + steps) % i + 1;
        if (pos <= zeroPos) {
            // Keep track of where 0 is.
            zeroPos++;
        }
        if (pos === zeroPos + 1) {
            // Remember the value inserted immediately after 0.
            result = i;
        }
    }
    return result;
};

const test = () => {
    const result1 = spinlock1(3);
    console.log(`[test] part1: ${result1}`);
    const result2 = spinlock2(3);
    console.log(`[test] part2: ${result2}`);
};

const real = () => {
    const result1 = spinlock1(301);
    console.log(`[real] part1: ${result1}`);
    const result2 = spinlock2(301);
    console.log(`[real] part2: ${result2}`);
};

test();
real();
