import * as fs from 'fs'

let input = fs.readFileSync('../../input.txt', 'utf8');
let output = input.trim().split('\n').map(line => getNumbers(line)).reduce((a, b) => a + b, 0);
console.log(output);

function getNumbers(line: string): number {
    const nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];
    let first = 0;
    let last = 0;
    for (let i = 0; i < line.length; i++) {
        let cur: number | null = null;
        const ch = line[i];
        if (!isNaN(parseInt(ch))) {
            cur = parseInt(ch);
        } else {
            for (let j = 0; j < nums.length; j++) {
                if (line.substring(i).startsWith(nums[j])) {
                    cur = j + 1;
                    break;
                }
            }
        }
        if (cur !== null) {
            if (first === 0) {
                first = cur;
                last = cur;
            } else {
                last = cur;
            }
        }
    }
    if (first === 0 || last === 0) {
        throw new Error("Invalid input");
    }
    return first * 10 + last;
}