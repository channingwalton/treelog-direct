# treelog-direct

Experimental Scala 3.3 rewrite of TreeLog's core idea: computations return both a result and a hierarchical log tree.

This project explores a smaller direct API than the original Cats transformer-based implementation. It is not source or binary compatible with the original library.

Original project: [lancewalton/treelog](https://github.com/lancewalton/treelog)

## Example

```scala
import treelog.direct.*

def root(p: Parameters): Logged[String, Double] =
  branch("Extracting root"):
    for
      num <- numerator(p)
      den <- denominator(p)
      r   <- success(num / den, r => s"Got root = numerator / denominator: $r")
    yield r
```

Failure is represented in both places:

- `Logged.result` is `Left(error)` when the computation stops.
- `Logged.tree` still contains the partial log, with failed branches marked.

## Development

```bash
sbt test
```
