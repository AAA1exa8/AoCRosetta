let input = await Bun.file('../../input.txt').text();
let engine: Engine = {nums: [], symbols: []};
input.split('\n').forEach((line, line_number) => parse_manual_line(engine, line.trim(), line_number));
let output = engine.symbols.map(g => {
    return engine.nums.filter(n => {
        return (g.position.y >= (n.position.y - 1))
            && (g.position.y <= (n.position.y + n.num_of_digits))
            && Math.abs(g.position.x - n.position.x) < 2;
    });
})
.filter(n => n.length === 2)
.map(n => n.reduce((acc, curr) => acc * curr.value, 1))
.reduce((acc, curr) => acc + curr, 0);

console.log(output);



type Engine = {
    nums: Number[],
    symbols: Symbol[],
}

type Number = {
    value: number,
    position: {x: number, y: number},
    num_of_digits: number
}

type Symbol = {
    position: {x: number, y: number},
}

function parse_manual_line(engine: Engine, line: string, line_number: number) {
    let recording = false;
    for(let i = 0; i < line.length; i++) {
        let char = line[i];
        if(char == '.') {
            recording = false;
            continue;
        }
        if(isNumeric(char)) {
            if(!recording) {
                recording = true;
                engine.nums.push({value: parseInt(char), position: {y: i, x: line_number}, num_of_digits: 1});
            } else {
                engine.nums[engine.nums.length - 1].value = engine.nums[engine.nums.length - 1].value * 10 + parseInt(char);
                engine.nums[engine.nums.length - 1].num_of_digits++;
            }
        }else if (char == '*') {
            engine.symbols.push({position: {y: i, x: line_number}});
            recording = false;
        }else {
            recording = false;
        }
    }
}

function isNumeric(n: string) {
    return !isNaN(parseFloat(n)) && isFinite(parseFloat(n));
  }
  