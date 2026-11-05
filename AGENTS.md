# Agent instructions

## Verify changes

Run `sbt commitCheck` to build and test the project. CI runs the same check.

`commitCheck` runs `clean;testFull`. In sbt 2, `test` is incremental and the build cache undoes `clean`, so `test` can report "No tests to run" after a change. `testFull` runs every test.

If sbt reports `Not a valid command: commitCheck` in 0 seconds, run `sbt shutdown`, then run `sbt commitCheck` again. The sbt 2 client can connect to a server that loaded only the meta-build.

## Compilation

- sbt-tpolecat sets `-Werror`, so every warning fails the build. This includes unused imports, parameters and local values.
- `-Wvalue-discard` and `-Wnonunit-statement` are on. Discard a non-`Unit` result with `val _ = ...`.
- `scalafmtOnCompile` is on, so each compilation formats the sources and can change the files that you edited.

## Tree invariants

`Tree.branch` and `Tree.combine` keep every tree normalized:

- An empty tree occurs only as the whole tree.
- A group occurs only as the root. It has two or more children, and no child is a group.
- A branch has one or more children, and no child is a group.
- The `successful` flag of a branch or group is the conjunction of the flags of its children.

## Tree traversals

The README promises stack-safe traversal. Write each traversal of `Tree` with an explicit stack, as `fold`, `same`, `hash`, `describe` and `renderState` do. Test each traversal on a tree with a depth of at least 10,000.
