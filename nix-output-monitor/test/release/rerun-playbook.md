# Rerun the release workflow against an existing release

Prerequisites: a fork of this repository with Actions enabled, a GitHub token
that can read and write releases, and network reachability to github.com.

1. Create the release tag v9.9.9 once, either by pushing the tag or by
   dispatching the release workflow with `tag = v9.9.9`.
2. Wait for the run to finish and record the release URL for v9.9.9.
3. Dispatch the release workflow again with `tag = v9.9.9`.
4. Wait for the run to finish.

Expected: both runs finish green. The second run updates the release at the
recorded URL and leaves the tag at the same commit. No duplicate release
appears.

Failure signal: the second run fails with a 422 `already_exists` error from
`softprops/action-gh-release`, which means the publish step tried to create a
release that already existed instead of updating it.
