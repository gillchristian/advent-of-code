const { readFileSync } = require('fs')

const input = readFileSync('./01-01.input.txt', 'utf-8')

const result = input
  .trim()
  .split('\n')
  .map(v => parseInt(v, 10))
  .reduce((acc, v) => acc + v, 0)

console.log({ result })
