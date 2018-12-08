const { readFileSync } = require('fs')

const input = readFileSync('./input/03.txt', 'utf-8')

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
  .map(() => new Array(width).fill([]))

lines
  .forEach(({ id, width, height, top, left }) => {
    const xFrom = top
    const xTo = top + height
    const yFrom = left
    const yTo = left + width

    for (let i = xFrom ; i < xTo ; i++) {
      for (let j = yFrom ; j < yTo ; j++) {
        canvas[i][j] = [ id, ...canvas[i][j] ]
      }
    }
  })

const cells = []
  .concat(...canvas)

const over = cells.filter(i => i.length > 1).length

console.log(`Overlapping inches: ${over}`)

const map = {}

canvas
  .forEach(line => {
    line.forEach((ids) => {
      if (ids.length === 0) {
        return
      }

      if (ids.length > 1) {
        ids.forEach(id => { map[id] = 'more' })
        return
      }

      if (map[ids[0]] === 'more') {
        return
      }

      map[ids[0]] = 'one'

    })
  })

const free = Object
  .entries(map)
  .find(kv => kv[1] === 'one')

console.log(`Free claim: #${free[0]}`)
