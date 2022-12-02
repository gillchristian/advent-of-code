import * as fs from "fs/promises";
import * as E from "fp-ts/Either";
import * as A from "fp-ts/ReadonlyArray";
import * as num from "fp-ts/number";
import { pipe } from "fp-ts/function";
import { sequenceT } from "fp-ts/Apply";
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

type Match = [Oponent, Self];

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

const parseMatch = (line: string): E.Either<string, Match> => {
  const [o, s] = words(line.trim());

  if (!o || !s) {
    return E.left(`Got invalid match: "${line}"`);
  }

  return sequenceT(E.Apply)(parseOponent(o), parseSelf(s));
};

// Score of Self: Rock - 1, Paper - 2, Scissors - 3
// Plus score for the outcome: lose - 0 if you lost, draw - 3, win - 6

const scoreSelf = (s: Self): number =>
  match(s)
    .with(Self.Rock, () => 1)
    .with(Self.Paper, () => 2)
    .with(Self.Scissors, () => 3)
    .exhaustive();

const score = ([o, s]: Match): number =>
  match([o, s])
    // Draw = 1 + Self
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

const parse = (input: string): E.Either<string, readonly Match[]> =>
  pipe(input.trim(), lines, E.traverseArray(parseMatch));

const INPUT_FILE = "src/days/02/input.txt";

fs.readFile(INPUT_FILE, "utf-8")
  .then(parse)
  .then(E.map(A.foldMap(num.MonoidSum)(score)))
  .then(
    E.match(
      (e) => console.error(`Failed to parse: ${e}`),
      (result) => console.log(`The score is: ${result}`)
    )
  );
