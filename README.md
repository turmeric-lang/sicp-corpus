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
- `(import (srfi 216))` -- the names SICP assumes from MIT Scheme (`true`,
  `false`, `nil`, `runtime`, `random`, `cons-stream`, `the-empty-stream`,
  `stream-null?`, `parallel-execute`, `test-and-set!`), which Turmeric
  ships as SRFI 216. The `#lang sicp` extras (`inc`, `dec`, `identity`,
  `amb`) come from `(sicp extras)` once that is in Turmeric `main`.
- Where the book redefines a procedure, the program does too, as printed.
  Where running the text needs something the book leaves to an exercise
  (an `or-gate`, `partial-sums`), that part is left out and the file's
  header says so; where it needs something the book leaves to the reader
  (more primitive procedures for the evaluator, a `rand-update`), it is
  supplied and marked "not the book's".
- `<section>.xfail` -- the program is blocked on an open Turmeric report; the
  first line names it. A mismatch passes as xfail, and a match fails with
  "delete the marker", so the day the fix lands the marker comes out.
  `<section>.xfail-compiled` / `.xfail-interpreted` do the same for one back
  end, when the blocking report is that back end's.
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
| 1.2 Procedures and the Processes They Generate | passing |
| 1.3 Formulating Abstractions with Higher-Order Procedures | passing |
| 2.1 Introduction to Data Abstraction | passing |
| 2.2 Hierarchical Data (2.2.1-2.2.3) | passing |
| 2.3 Symbolic Data | passing |
| 3.1 Assignment and Local State | passing |
| 3.3 Modeling with Mutable Data | passing |
| 3.4 Concurrency | passing (invariants, not exact output) |
| 3.5 Streams | passing |
| 4.1 The Metacircular Evaluator (4.1.1-4.1.7, with the analyzing evaluator) | passing |
| 4.2 Lazy Evaluation (the lazy evaluator, 4.2.3's lazy lists) | passing |
| 4.3 Nondeterministic Computing (the `amb` evaluator and its examples) | passing |
| 4.4 Logic Programming (the query system on the Microshaft data base) | passing |
| 5.2 A Register-Machine Simulator (with 5.1's gcd, factorial and Fibonacci machines) | passing |
| 5.5 Compilation (the compiler, on 5.4's explicit-control evaluator) | passing |

Writing the corpus found Turmeric defects, each fixed on `main`
([turmeric#1091](https://github.com/turmeric-lang/turmeric/pull/1091)):
[redefinition](https://github.com/turmeric-lang/turmeric/blob/main/docs/archive/r7rs-program-redefinition-refused.md)
(SICP refines procedures by defining them again),
[`eq?` on internal procedures](https://github.com/turmeric-lang/turmeric/blob/main/docs/archive/r7rs-internal-procedure-value-not-eq.md)
(3.3.5's constraints), and proper tail calls for the `amb` evaluator's
continuation-passing style, both
[under the interpreter](https://github.com/turmeric-lang/turmeric/blob/main/docs/archive/turi-tail-call-through-procedure-value-grows-stack.md)
and [compiled](https://github.com/turmeric-lang/turmeric/blob/main/docs/archive/r7rs-tail-call-through-static-call-not-proper.md).
A section blocked on a new defect gets a `.xfail` marker naming its report.

Each chapter-4 evaluator runs its own driver loop on the book's sessions,
fed from a string in the program through `current-input-port`.

Not covered: 5.3 (storage allocation; its code is register-machine
fragments with nothing of its own to run) and 5.4 on its own (it runs as
part of 5.5, with the book's interpreted results checked there). The
picture language of 2.2.4 is out of scope.
