function findElfWithMostCalories(input: string): number {
    const inventories = input.split('\n\n');

    const totalCalories = inventories.map(inventory => {
        const calories = inventory.split('\n').map(Number);
        return calories.reduce((a, b) => a + b, 0);
    });
    const maxCalories = Math.max(...totalCalories);

    return maxCalories;
}

let input = await Bun.file('../../input.txt').text();
let answer = findElfWithMostCalories(input);
console.log(answer);