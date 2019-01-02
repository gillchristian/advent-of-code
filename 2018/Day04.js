const { readFileSync } = require('fs')

const { create, env } = require('sanctuary')
const R = require('ramda')

// -------------------------------------

const S = create({
  checkTypes: process.env.ENV !== 'prod',
  env,
})

// -------------------------------------

// tupleToPair :: [String] -> Pair String String
const tupleToPair = ([a, b]) => S.Pair (a) (b)

// map2 :: (Functor f) => (a -> b -> c) -> f a -> f b -> f c
const map2 = (f) => (m1) => (m2) =>
  S.chain 
    (v1 => S.map (v2 => f(v1) (v2)) (m2))
    (m1)

// -------------------------------------

const FALL_SLEEP = 'FALL SLEEP'
const WAKE_UP = 'WAKE UP'
const GUARD = 'GUARD'

// type Sleep = { type :: FALL_SLEEP }
// type Awake = { type :: WAKE_UP }
// type Guard = { type :: GUARD, id :: Int }

// type Event =
//     Sleep
//   | Awake
//   | Guard

// parseGuard :: String -> Either String Guard
const parseGuard = S.pipe ([
  S.match (/[0-9]+/),
  S.map (S.prop ('match')),
  S.chain (S.parseInt (10)),
  S.map (id => ({ type: GUARD, id })),
  S.maybeToEither ('Nothing > parseGuard'),
])

// parseEvent :: String -> Either String Event
const parseEvent = R.cond ([
  [S.test (/falls/), () => S.Right({ type: FALL_SLEEP }) ],
  [S.test (/wakes/), () => S.Right({ type: WAKE_UP }) ],
  [R.T, parseGuard ],
])

// splitOnDateAndEvent :: String -> [String]
const splitOnDateAndEvent = S.splitOn('] ')

// toPair :: String -> Pair String String
const toPair = S.compose (tupleToPair) (splitOnDateAndEvent)

// split :: String -> Either String (Pair String String)
const split = S.pipe ([
  S.stripPrefix ('['),
  S.map (toPair),
  S.maybeToEither ('Nothing > split'),
])

// getDate :: Maybe (Pair String String) -> Either String Date
const getDate = S.pipe ([
  S.map (S.fst),
  S.eitherToMaybe,
  S.chain (S.parseDate),
  S.maybeToEither ('Nothing > getDate'),
])

// getEvent :: Maybe (Pair String String) -> Either String Event
const getEvent = S.pipe ([
  S.map (S.snd),
  S.chain (parseEvent),
  S.mapLeft (err => `${err || ''} > getEvent`),
])

// dateAndEvent :: Date -> Event -> { date :: Date, event :: Event }
const dateAndEvent = (date) => (event) => ({ date, event })

const parse = (line) => {
  const eitherPair = split(line)

  const eitherDate = getDate(eitherPair)
  const eitherEvent = getEvent(eitherPair)

  return map2
    (dateAndEvent)
    (eitherDate)
    (eitherEvent)
}

// -------------------------------------

const path = process.env.ENV === 'prod' ? 'input/04.txt' : '04.input.test.txt'

const input = readFileSync(path, 'utf-8')

const lines = S.pipe ([
  S.lines,
  S.sort,
  S.map (parse),
  S.traverse (Array) (S.I),
]) (input)

console.log(lines)
