import * as fs from "fs/promises";
import * as E from "fp-ts/Either";
import * as A from "fp-ts/ReadonlyArray";
import * as num from "fp-ts/number";
import { pipe } from "fp-ts/function";
import { sequenceT, sequenceS } from "fp-ts/Apply";
import { match } from "ts-pattern";

import { lines, words } from "../../lib/string";

const enum Oponent {
  Rock = "A",
  Paper = "B",
  Scissors = "C",
}

const enum Self {
  Rock = "X",
  Paper = "Y",
  Scissors = "Z",
}

const enum ExpectedResult {
  Lose = "X",
  Draw = "Y",
  Win = "Z",
}

type Match = [Oponent, Self];
type Strategy = [Oponent, ExpectedResult];

const parseOponent = (c: string): E.Either<string, Oponent> =>
  match(c)
    .with("A", () => E.right(Oponent.Rock))
    .with("B", () => E.right(Oponent.Paper))
    .with("C", () => E.right(Oponent.Scissors))
    .otherwise(() => E.left(`Got invalid oponent ${c}`));

const parseSelf = (c: string): E.Either<string, Self> =>
  match(c)
    .with("X", () => E.right(Self.Rock))
    .with("Y", () => E.right(Self.Paper))
    .with("Z", () => E.right(Self.Scissors))
    .otherwise(() => E.left(`Got invalid self "${c}"`));

const parseExpectedResult = (c: string): E.Either<string, ExpectedResult> =>
  match(c)
    .with("X", () => E.right(ExpectedResult.Lose))
    .with("Y", () => E.right(ExpectedResult.Draw))
    .with("Z", () => E.right(ExpectedResult.Win))
    .otherwise(() => E.left(`Got invalid strategy "${c}"`));

const parseBoth = (
  line: string
): E.Either<string, { match: Match; strategy: Strategy }> => {
  const [o, s] = words(line.trim());

  if (!o || !s) {
    return E.left(`Got invalid match: "${line}"`);
  }

  return sequenceS(E.Apply)({
    match: sequenceT(E.Apply)(parseOponent(o), parseSelf(s)),
    strategy: sequenceT(E.Apply)(parseOponent(o), parseExpectedResult(s)),
  });
};

const scoreSelf = (s: Self): number =>
  match(s)
    .with(Self.Rock, () => 1)
    .with(Self.Paper, () => 2)
    .with(Self.Scissors, () => 3)
    .exhaustive();

const scoreMatch = ([o, s]: Match): number =>
  match([o, s])
    // Draw = 3 + Self
    .with(
      [Oponent.Rock, Self.Rock],
      [Oponent.Paper, Self.Paper],
      [Oponent.Scissors, Self.Scissors],
      () => 3 + scoreSelf(s)
    )

    // Win  = 6 + Self
    .with(
      [Oponent.Rock, Self.Paper],
      [Oponent.Paper, Self.Scissors],
      [Oponent.Scissors, Self.Rock],
      () => 6 + scoreSelf(s)
    )

    // Lose = Self
    .with(
      [Oponent.Rock, Self.Scissors],
      [Oponent.Paper, Self.Rock],
      [Oponent.Scissors, Self.Paper],
      () => scoreSelf(s)
    )

    .otherwise(() => 0);

const playStrategy = ([o, s]: Strategy): number =>
  match([s, o])

    // Draw = 3 + Self
    .with([ExpectedResult.Draw, Oponent.Rock], () => 3 + scoreSelf(Self.Rock))
    .with([ExpectedResult.Draw, Oponent.Paper], () => 3 + scoreSelf(Self.Paper))
    .with([ExpectedResult.Draw, Oponent.Scissors], () => 3 + scoreSelf(Self.Scissors))

    // Win  = 6 + Self
    .with([ExpectedResult.Win, Oponent.Rock], () => 6 + scoreSelf(Self.Paper))
    .with([ExpectedResult.Win, Oponent.Paper], () => 6 + scoreSelf(Self.Scissors))
    .with([ExpectedResult.Win, Oponent.Scissors], () => 6 + scoreSelf(Self.Rock))

    // Lose = Self
    .with([ExpectedResult.Lose, Oponent.Rock], () => scoreSelf(Self.Scissors))
    .with([ExpectedResult.Lose, Oponent.Paper], () => scoreSelf(Self.Rock))
    .with([ExpectedResult.Lose, Oponent.Scissors], () => scoreSelf(Self.Paper))

    .otherwise(() => 0);

const parse = (
  input: string
): E.Either<string, readonly { match: Match; strategy: Strategy }[]> =>
  pipe(input.trim(), lines, E.traverseArray(parseBoth));

const INPUT_FILE = "src/days/02/input.txt";

fs.readFile(INPUT_FILE, "utf-8")
  .then(parse)
  .then(
    E.map((xs): [number, number] => [
      pipe(
        xs,
        A.foldMap(num.MonoidSum)((x) => scoreMatch(x.match))
      ),
      pipe(
        xs,
        A.foldMap(num.MonoidSum)((x) => playStrategy(x.strategy))
      ),
    ])
  )
  .then(
    E.match(
      (e) => console.error(`Failed to parse: ${e}`),
      ([res1, res2]) => {
        console.log(`The score of all matches:       ${res1}`);
        console.log(`The score with the strategy is: ${res2}`);
      }
    )
  );
