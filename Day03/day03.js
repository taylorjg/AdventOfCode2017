// --------------------------------------------------------------------------------

const same = v => v;
const inc = v => v + 1
const dec = v => v - 1

const xop = (len, i) => {
    const q = Math.floor(i / len);
    switch (q) {
        case 0: return same;
        case 1: return dec;
        case 2: return same;
        case 3: return inc;
    }
};

const yop = (len, i) => {
    const q = Math.floor(i / len);
    switch (q) {
        case 0: return inc;
        case 1: return same;
        case 2: return dec;
        case 3: return same;
    }
};

const dumpCoords = side => {
    const len = side - 1;
    const n = len / 2;
    const numSteps = (side * side) - ((side - 2) * (side - 2));
    let x = n;
    let y = -n;
    const coords = [];
    for (let i = 0; i < numSteps; i++) {
        x = xop(len, i)(x);
        y = yop(len, i)(y);
        coords.push([x, y]);
    }
    console.log(JSON.stringify(coords));
};

// --------------------------------------------------------------------------------

// 5  4  3
// 6  1  2
// 7  8  9

const expected3x3 = [
    [1, 0], // 2    
    [1, 1], // 3    
    [0, 1], // 4    
    [-1, 1], // 5
    [-1, 0], // 6
    [-1, -1], // 7
    [0, -1], // 8
    [1, -1], // 9
];

dumpCoords(3);
console.log(JSON.stringify(expected3x3));

// --------------------------------------------------------------------------------

// 17  16  15  14  13
// 18   5   4   3  12
// 19   6   1   2  11
// 20   7   8   9  10
// 21  22  23  24  25

const expected5x5 = [
    [2, -1], // 10
    [2, 0], // 11
    [2, 1], // 12
    [2, 2], // 13
    [1, 2], // 14
    [0, 2], // 15
    [-1, 2], // 16
    [-2, 2], // 17
    [-2, 1], // 18
    [-2, 0], // 19
    [-2, -1], // 20
    [-2, -2], // 21
    [-1, -2], // 22
    [0, -2], // 23
    [1, -2], // 24
    [2, -2]  // 25
];

dumpCoords(5);
console.log(JSON.stringify(expected5x5));

// --------------------------------------------------------------------------------

// 37  36  35  34  33  32  31
// 38  17  16  15  14  13  30
// 39  18   5   4   3  12  29
// 40  19   6   1   2  11  28
// 41  20   7   8   9  10  27
// 42  21  22  23  24  25  26
// 43  44  45  46  47  48  49

const expected7x7 = [
    [3, -2], // 26
    [3, -1], // 27
    [3, 0], // 28
    [3, 1], // 29
    [3, 2], // 30
    [3, 3], // 31
    [2, 3], // 32
    [1, 3], // 33
    [0, 3], // 34
    [-1, 3], // 35
    [-2, 3], // 36
    [-3, 3], // 37
    [-3, 2], // 38
    [-3, 1], // 39
    [-3, 0], // 40
    [-3, -1], // 41
    [-3, -2], // 42
    [-3, -3], // 43
    [-2, -3], // 44
    [-1, -3], // 45
    [0, -3], // 46
    [1, -3], // 47
    [2, -3], // 48
    [3, -3]  // 49
];

dumpCoords(7);
console.log(JSON.stringify(expected7x7));

// --------------------------------------------------------------------------------

dumpCoords(9);
dumpCoords(11);
dumpCoords(13);
