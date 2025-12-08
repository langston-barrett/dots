# Release

To create a release:

- Create branch with a name starting with `release`
- Update `CHANGELOG.md`
- Update the version numbers in `Cargo.toml` files

  ```sh
  find . -type f -name "Cargo.toml" -print0 | \
    xargs -0 sed -E -i 's/^version = "U.V.W"$/version = "X.Y.Z"/'
  ```

- Run `cargo build --release`
- Commit all changes and push the release branch
- Check that CI was successful on the release branch
- Merge the release branch to `main`
- `git checkout main && git pull origin && git tag -a vX.Y.Z -m vX.Y.Z && git push --tags`
- Verify that the release artifacts work as intended
- Check that the crates were properly uploaded to crates.io
- Release the pre-release created by CI
