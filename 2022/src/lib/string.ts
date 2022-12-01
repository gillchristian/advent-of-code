import * as E from "fp-ts/Either";

export const words = (str: string): readonly string[] => str.split(" ");

export const lines = (str: string): readonly string[] => str.split("\n");

export const parseInt_ = (n_: string): E.Either<string, number> => {
  const n = parseInt(n_.trim(), 10);

  return Number.isNaN(n) ? E.left(`Not a number ${n_}`) : E.right(n);
};
