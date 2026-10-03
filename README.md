# sicp-corpus

The code from *Structure and Interpretation of Computer Programs* (SICP),
section by section, run against [Turmeric](https://github.com/turmeric-lang/turmeric)'s
R7RS Scheme dialect on both of its back ends: compiled (`tur run`) and the
interpreter (`tur --interpret`).

It exists so that a student working through SICP in Turmeric finds the book's
code working, and so that a change to Turmeric that breaks it is caught here
first. The student-facing guide is Turmeric's
[sicp-guide](https://github.com/turmeric-lang/turmeric/blob/main/docs/guides/sicp-guide.md);
the plan this repo implements is
[r7rs-srfi-18-216-sicp-plan](https://github.com/turmeric-lang/turmeric/blob/main/docs/upcoming/r7rs-srfi-18-216-sicp-plan.md)
(section D3).

This is a separate repository because its content is adapted from the book,
which is licensed CC BY-SA 4.0, and ShareAlike content stays out of the
Turmeric tree. See [NOTICE](NOTICE) and [LICENSE](LICENSE).

## Running it

```sh
TUR=/path/to/tur bash run.sh             # everything
TUR=/path/to/tur bash run.sh ch1/1.1     # one section
```

Each program runs compiled and interpreted, and both outputs must equal its
`.expected` file. The summary line reads
`P passed, X xfail, F failed, S skipped`; the script exits non-zero on any
failure.

## Layout and conventions

- `ch<N>/sec-<section>.scm` -- one book section (`ch1/sec-1.1.scm`), transcribed in the book's order. The `sec-` keeps the file name from starting with a digit, which Turmeric cannot compile yet when the program imports a library ([report](https://github.com/turmeric-lang/turmeric/blob/main/docs/reported/r7rs-program-file-named-with-leading-digit-fails-to-compile.md)).
  Every expression whose value the book states is printed, so `.expected`
  holds the book's own answers. Each `.expected` value is checked against
  the book by hand when it is added; never regenerate one blindly from
  `tur` output.
- `corpus/prelude.scm` -- `(corpus prelude)`: the names SICP assumes from MIT
  Scheme (`true`, `false`, `nil`, `runtime`, `random`, `cons-stream`,
  `the-empty-stream`, `stream-null?`). It is replaced by
  `(import (srfi 216))` once Turmeric ships SRFI 216, and by
  `(sicp extras)` for `inc`/`dec`/`identity`/`amb`.
- `<section>.xfail` -- the program is blocked on an open Turmeric report; the
  first line names it. A mismatch passes as xfail, and a match fails with
  "delete the marker", so the day the fix lands the marker comes out.
- `<section>.compiled-only` / `.interp-only` -- skip the other back end; the
  first line says why.
- No exercise solutions. Exercises are the reader's work.

## CI

`.github/workflows/ci.yml` builds Turmeric at the current `main` and runs the
corpus on Linux and macOS, on every push, every pull request, and daily. A
red run prints the Turmeric sha it built: two runs hours apart can use
different compilers, so compare the sha before blaming a change here.

## Coverage

| Section | Status |
|---|---|
| 1.1 The Elements of Programming | passing |

Priorities, from the plan: chapters 1-3 and 3.5 streams; then 4.1 (the
metacircular evaluator, which needs a Turmeric fix and will land as xfail);
then 4.3 `amb`, 4.4 the query system, 5.2 the register-machine simulator and
5.5 the compiler; 3.4 concurrency once SRFI 18 lands. The picture language
of 2.2.4 is out of scope.
