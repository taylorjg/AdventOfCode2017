const isPrime = n => {
    for (let i = 2; i <= Math.sqrt(n); i++) {
        if (n % i === 0) return false;
    }
    return n !== 1;
};

console.log(Array.from(Array(50).keys()).filter(isPrime));

const b = 65 * 100 - (-100000);
const c = b - (-17000);
let h = 0;
console.log(`b: ${b}; c: ${c}`);
for (let n = b; n <= c; n += 17) {
    if (!isPrime(n)) {
        h++;
    }
}
console.log(`h: ${h}`);
