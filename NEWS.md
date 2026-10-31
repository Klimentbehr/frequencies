# frequencies 0.2.0

## Bug fixes
* `freq_vect()`: the total row no longer turns `Count`, `Percentage` and `Cum.` into character
  columns. They stay numeric; only the first column is character (so it can hold "Total").
* `freq_vect()`: `Cum.` is computed from unrounded counts, so it always ends at exactly 100
  instead of drifting to 99.9 / 100.1.
* `freq_vect()`: vectors of length 1 are now accepted; non-logical flag arguments raise an error
  instead of being silently replaced with `TRUE`.
* `freq_two_vects()`: the `exists(deparse(substitute(df)))` check is gone, so `df` can be any
  expression (`df[df$x > 1, ]`, the result of a pipe, a function argument ...). `df` must now be a
  data frame rather than any list.
* `freq_two_vects()`: columns may be given quoted or unquoted.
* Removed use of `dplyr::arrange_()` and `dplyr::as_data_frame()`, which are defunct/deprecated and
  made the package fail on current dplyr.

## New
* `freq_vect(include_na = )` to count or drop missing values (previously always counted).

## Performance
* Both functions are now base R (single `unique()`/`match()`/`tabulate()` pass) and the package
  no longer depends on dplyr. `freq_two_vects()` no longer builds the full cross-tab, so memory is
  proportional to the combinations that occur.

## Behaviour changes
* `freq_two_vects()` returns only combinations that occur in the data (no zero-count rows).
* `freq_vect()` total row has numeric `Count`/`Percentage`/`Cum.`.
