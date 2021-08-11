# amazonka-contrib-rds-utils

This library provides a Haskell equivalent of `aws rds generate-db-auth-token`
[CLI utility](https://awscli.amazonaws.com/v2/documentation/api/latest/reference/rds/generate-db-auth-token.html)
built on top of [`amazonka`](https://hackage.haskell.org/package/amazonka) library.


## Developer notes

The following commands assume they are run from the provided Nix shell

### Building the project

```
make build
```