# tfautograph 0.3.2

* Added back compatibility with R-3.3 and R-3.4

# tfautograph 0.3.1

* Fixed issue that prevented the package from being loaded by `devtools::load_all()`

# tfautograph 0.3.0

* Improved handling of `shape_invariants` supplied to `ag_while_opts`. A named list of user variable shapes can be passed directly now, without requiring users to manually specify shapes of internal loop tracking tensors.
* Deprecated `back_prop` arg in `ag_while_opts()`
* Move 'tensorflow' package from 'Imports' to 'Suggests' to avoid circular dependency.
* Added a `NEWS.md` file to track changes to the package.
