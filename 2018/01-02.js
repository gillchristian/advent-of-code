const { readFileSync } = require('fs')

const input = readFileSync('./01-01.input.txt', 'utf-8')

const find = (count, results) => {
  const result = input
    .trim()
    .split('\n')
    .map(v => parseInt(v, 10))
    .reduce(
      (acc, v) => {
        acc.count = acc.count + v

        if (acc.results[acc.count]) {
          console.log(acc.count)
          process.exit(0)
        }

        acc.results[acc.count] = true
        return acc
      },
      { count, results, twice: false },
    )

  if (result.twice !== false) {
    return result.twice
  }

  return find(result.count, result.results)
}

find(0, {})
