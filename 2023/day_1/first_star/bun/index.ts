let input = await Bun.file('../../input.txt').text();
let output = input.trim().split('\n').map(line => {
    let firstDigit: string = '';
    let lastDigit: string = '';
    line.split('').forEach((char) => {
        if (!isNaN(parseInt(char))) {
            if (firstDigit === '') {
                firstDigit = char;
            }
            lastDigit = char;
        }
    });
    return parseInt(`${firstDigit}${lastDigit}`);
}).reduce((a, b) => a + b, 0);
console.log(output);