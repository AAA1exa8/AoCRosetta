import * as fs from 'fs';

let input = fs.readFileSync('../../input.txt', 'utf8');
let output = input.trim().split('\n').map((line) => {
    let [id, values] = line.split(': ');
    let rounds = values.split(';');
    let game: Game = {
        id: parseInt(id.split(' ')[1]),
        values: [],
    };
    for (let round of rounds) {
        let colors = round.split(',');
        let r = {
            red: null as number | null,
            blue: null as number | null,
            green: null as number | null,
        };
        for (let color of colors) {
            let [number, colorName] = color.trim().split(" ");
            switch (colorName) {
                case "red":
                    r.red = parseInt(number);
                    break;
                case "blue":
                    r.blue = parseInt(number);
                    break;
                case "green":
                    r.green = parseInt(number);
                    break;
                default:
                    throw new Error(`Invalid color: ${colorName}`);
            }
        }
        game.values.push(r);
    }
    return game;
}).filter((game) => {
    let valid = true;
    game.values.forEach((value) => {
        if ((value.red ?? 0) > 12 || (value.blue ?? 0) > 14 || (value.green ?? 0) > 13) {
            valid = false;
        }
    })
    return valid;
}).reduce((output, game) => output + game.id, 0)
console.log(output);


type Game = {
    id: number,
    values: {
        red: number | null,
        blue: number | null,
        green: number | null
    }[]
}