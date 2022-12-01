import * as fs from "fs/promises";
import * as E from "fp-ts/Either";
import * as A from "fp-ts/ReadonlyArray";
import * as num from "fp-ts/number";
import * as M from "fp-ts/Monoid";
import { pipe } from "fp-ts/function";

import { sum } from "../../lib/array";
import { lines, parseInt_ } from "../../lib/string";

const parse = (
  input: string
): E.Either<string, readonly (readonly number[])[]> =>
  pipe(
    input.trim().split("\n\n"),
    E.traverseArray((s) => pipe(s.trim(), lines, E.traverseArray(parseInt_)))
  );

const INPUT_FILE = "src/days/01/input.txt";

const findTop3 = (ns: readonly (readonly number[])[]) =>
  pipe(
    ns,
    A.reduce<readonly number[], [number, number, number]>(
      [0, 0, 0],
      ([a, b, c], ns) => {
        const n = sum(ns);

        return n > a
          ? [n, a, b]
          : n > b
          ? [a, n, b]
          : n > c
          ? [a, b, n]
          : [a, b, c];
      }
    )
  );

fs.readFile(INPUT_FILE, "utf-8")
  .then(parse)
  .then((ns) =>
    pipe(
      ns,
      E.map((ns) => ({
        top: A.foldMap(M.max(num.Bounded))(sum)(ns),
        top3: findTop3(ns),
      })),
      E.match(
        (e) => console.log(`Error: ${e}`),
        ({ top, top3 }) => {
          console.log(`Top:         ${top}`);
          console.log(`Top 3 (sum): ${sum(top3)}`);
          console.log(`Top 3:       ${top3}`);
        }
      )
    )
  );
