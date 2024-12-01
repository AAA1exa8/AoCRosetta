let input = await Bun.file('../../input.txt').text();
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
}).reduce((output, game) => {
    let max_red = game.values.map((r) => r.red).reduce((a, b) => Math.max((a ?? 0), (b ?? 0)),0) as number;
    let max_blue = game.values.map((r) => r.blue).reduce((a, b) => Math.max((a ?? 0), (b ?? 0)),0) as number;
    let max_green = game.values.map((r) => r.green).reduce((a, b) => Math.max((a ?? 0), (b ?? 0)),0) as number;
    return (max_blue * max_green * max_red) + output;
}, 0)
console.log(output);


type Game = {
    id: number,
    values: {
        red: number | null,
        blue: number | null,
        green: number | null
    }[]
}