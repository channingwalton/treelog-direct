# treelog-direct

[![Scala CI](https://github.com/channingwalton/treelog-direct/actions/workflows/scala.yml/badge.svg)](https://github.com/channingwalton/treelog-direct/actions/workflows/scala.yml)

Experimental Scala 3 rewrite of [lancewalton/treelog](https://github.com/lancewalton/treelog)'s core idea: computations return both a result and a hierarchical log tree.

This project explores a smaller direct API than the original Cats transformer-based implementation. It is not source or binary compatible with the original library.

## Modules

- `treelog-direct-core`: pure `Logged[E, A]` and `Tree`.
- `treelog-direct-cats`: `LoggedT[F, E, A]` for projects already working in an effect type.

## Example

```scala
import treelog.direct.*

final case class Parameters(a: Double, b: Double, c: Double)

def root(p: Parameters): Logged[String, Double] =
  branch("Extracting root"):
    for
      num <- numerator(p)
      den <- denominator(p)
      r   <- success(num / den, r => s"Got root = numerator / denominator: $r")
    yield r

def numerator(p: Parameters): Logged[String, Double] =
  branch("Calculating Numerator"):
    for
      det    <- determinant(p)
      sqrt   <- sqrtDeterminant(det)
      b      <- success(p.b, b => s"Got b: $b")
      minusB <- success(-b, b => s"Got -b: $b")
      sum    <- success(minusB + sqrt, s => s"Got -b + sqrt(determinant): $s")
    yield sum

def denominator(p: Parameters): Logged[String, Double] =
  branch("Calculating Denominator"):
    for
      a    <- success(p.a, a => s"Got a: $a")
      twoA <- success(2 * a, twoA => s"Got 2a: $twoA")
    yield twoA

def determinant(p: Parameters): Logged[String, Double] =
  branch("Calculating Determinant"):
    for
      b2 <- bSquared(p)
      ac <- fourac(p)
      d  <- success(b2 - ac, d => s"Got b^2 - 4ac: $d")
    yield d

def bSquared(p: Parameters): Logged[String, Double] =
  branch("Calculating b^2"):
    for
      b  <- success(p.b, b => s"Got b: $b")
      b2 <- success(b * b, b2 => s"Got b^2: $b2")
    yield b2

def fourac(p: Parameters): Logged[String, Double] =
  branch("Calculating 4ac"):
    for
      a  <- success(p.a, a => s"Got a: $a")
      c  <- success(p.c, c => s"Got c: $c")
      ac <- success(4 * a * c, ac => s"Got 4ac: $ac")
    yield ac

def sqrtDeterminant(det: Double): Logged[String, Double] =
  branch("Calculating sqrt(determinant)"):
    for
      _ <- Logged.require(
        det >= 0,
        error = s"Negative determinant: $det",
        successLabel = s"Determinant ($det) is >= 0",
        failureLabel = s"Determinant ($det) is < 0"
      )
      r <- success(Math.sqrt(det), r => s"Got sqrt(determinant): $r")
    yield r
```

Failure is represented in both places:

- `Logged.result` is `Left(error)` when the computation stops.
- `Logged.tree` still contains the partial log, with failed branches marked.

Success:

```scala
val output = root(Parameters(2, 5, 3))

output.result
// Right(-1.0)

output.tree.render
// Extracting root
//   Calculating Numerator
//     Calculating Determinant
//       Calculating b^2
//         Got b: 5.0
//         Got b^2: 25.0
//       Calculating 4ac
//         Got a: 2.0
//         Got c: 3.0
//         Got 4ac: 24.0
//       Got b^2 - 4ac: 1.0
//     Calculating sqrt(determinant)
//       Determinant (1.0) is >= 0
//       Got sqrt(determinant): 1.0
//     Got b: 5.0
//     Got -b: -5.0
//     Got -b + sqrt(determinant): -4.0
//   Calculating Denominator
//     Got a: 2.0
//     Got 2a: 4.0
//   Got root = numerator / denominator: -1.0
```

Failure:

```scala
val output = root(Parameters(2, 5, 10))

output.result
// Left("Negative determinant: -55.0")

output.tree.render
// Extracting root: Failed
//   Calculating Numerator: Failed
//     Calculating Determinant
//       Calculating b^2
//         Got b: 5.0
//         Got b^2: 25.0
//       Calculating 4ac
//         Got a: 2.0
//         Got c: 10.0
//         Got 4ac: 80.0
//       Got b^2 - 4ac: -55.0
//     Calculating sqrt(determinant): Failed
//       Determinant (-55.0) is < 0: Failed
```

## Public tree traversal

Use `Tree.fold[A]` to build reports, custom renderers, or integration data without parsing `render` or accessing private node types:

```scala
val failedLeaves = output.tree.fold[Int](
  empty = 0,
  leaf = (_, success) => if success then 0 else 1,
  branch = (_, _, children) => children.sum,
  group = (_, children) => children.sum
)
// 1 for the failure example above
```

- `empty` is a by-name result, evaluated only for an empty tree.
- `leaf` receives `(label, success)`.
- `branch` receives `(label, success, children: Vector[A])`.
- `group` receives `(success, children: Vector[A])` for anonymous siblings created by `Tree.combine`.

Handlers run once per node in depth-first, left-to-right post-order: children are folded before their parent, and child results retain their original order. Branch and group success flags describe the original subtree, independently of the folded results. Labels are passed unchanged, including nulls and line separators.

The fold observes the normalized tree: empty children are removed, empty branches disappear, combined groups are flattened, and wrapping a group in a branch makes its members the branch's direct children.

Traversal is stack-safe for deeply nested trees and takes linear time in the number of nodes, excluding handler work. Handlers remain responsible for their own recursion. Handler exceptions propagate immediately and stop traversal before later nodes are visited.

## Cats

```scala
import cats.effect.IO
import treelog.direct.cats.LoggedT

val output: LoggedT[IO, String, Int] =
  LoggedT.branch("Adding"):
    for
      one <- LoggedT.liftF(IO.pure(1), value => s"Got one: $value")
      two <- LoggedT.liftF(IO.pure(2), value => s"Got two: $value")
      sum <- LoggedT.success[IO, String, Int](one + two, value => s"Got sum: $value")
    yield sum

output.value
// IO(Logged(Right(3), ...))
```

## Development

```bash
sbt commitCheck
```
