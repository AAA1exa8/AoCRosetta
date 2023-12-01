function findTopThreeElvesWithMostCalories(input: string): number {
    const inventories = input.split('\n\n');

    const totalCalories = inventories.map(inventory => {
        const calories = inventory.split('\n').map(Number);
        return calories.reduce((a, b) => a + b, 0);
    });

    totalCalories.sort((a, b) => b - a);

    const sumOfTopThree = totalCalories.slice(0, 3).reduce((a, b) => a + b, 0);

    return sumOfTopThree;
}

let input = await Bun.file('../../input.txt').text();
let answer = findTopThreeElvesWithMostCalories(input);
console.log(answer);