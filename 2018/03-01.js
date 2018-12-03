const { readFileSync } = require('fs')

const input = readFileSync('./03.input.txt', 'utf-8')

const sToI = v => parseInt(v, 10)

const parseLine = (line) => {
  const [id, rest] = line
    .replace('#', '')
    .replace(/\s/g, '')
    .split('@')

  const [xy, wh]= rest.split(':')

  const [left, top] = xy.split(',').map(sToI)
  const [width, height] = wh.split('x').map(sToI)

  return { id: sToI(id), top, left, width, height }
}

const lines = input
  .trim()
  .split('\n')
  .map(parseLine)

const height = lines
  .reduce(
    (max, { top, height }) => {
      const h = top + height

      return h > max ? h : max 
    },
    -Infinity,
  )

const width = lines
  .reduce(
    (max, { left, width }) => {
      const w = left + width

      return w > max ? w : max 
    },
    -Infinity,
  )

const canvas = new Array(height)
  .fill(0)
  .map(() => new Array(width).fill(0))

lines
  .forEach(({ width, height, top, left }) => {
    const xFrom = top
    const xTo = top + height
    const yFrom = left
    const yTo = left + width

    for (let i = xFrom ; i < xTo ; i++) {
      for (let j = yFrom ; j < yTo ; j++) {
        canvas[i][j] = canvas[i][j] + 1
      }
    }
  })

const cells = []
  .concat(...canvas)

console.log(
  JSON.stringify({ cells: cells.filter(c => c > 1).length }),
)
