const spinlock = steps => {
    const buffer = [0];
    let pos = 0;
    for (let i = 1; i <= 2017; i++) {
        const len = buffer.length;
        const insertPos = (pos + steps) % len;
        pos = insertPos + 1;
        buffer.splice(pos, 0, i);
    }
    return buffer[pos + 1];
};

const test = () => {
    const result = spinlock(3);
    console.log(`[test] part1: ${result}`);
};

const real = () => {
    const result = spinlock(301);
    console.log(`[real] part1: ${result}`);
};

test();
real();
