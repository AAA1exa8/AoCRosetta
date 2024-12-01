import * as fs from 'fs';

let input = fs.readFileSync('../../input.txt', 'utf8');
let Engine: Engine = {nums: [], symbols: []};
input.split('\n').forEach((line, line_number) => parse_manual_line(Engine, line.trim(), line_number));
let output = Engine.nums.filter(n => {
    return Engine.symbols.some(s => {
        return (s.position.y >= (n.position.y - 1))
            && (s.position.y <= (n.position.y + n.num_of_digits))
            && (Math.abs(s.position.x - n.position.x) <= 1)
    })
}).map(n => n.value).reduce((a, b) => a + b, 0);
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
        }else{
            engine.symbols.push({position: {y: i, x: line_number}});
            recording = false;
        }
    }
}

function isNumeric(n: string) {
    return !isNaN(parseFloat(n)) && isFinite(parseFloat(n));
  }
  