# rmmc 0.2.0

## Rewrite

* All seven measures now share one sparse computation. Firms are paired only
  within the markets they share, instead of crossing every firm with every
  other firm and every market. Results that took hours now take seconds.
* The code is organised in shared files (`input.R`, `pairs.R`, `measures.R`,
  `levels.R`); each exported function only defines its measure.

## Changed results

These follow Gimeno & Jeong (2001), Appendix A:

* `mmc_chen()`: `P_n` is now the total of market *n* over all firms at that
  date. Before, it was the total over all markets (and all dates), so values
  were too small and not comparable across markets.
* `mmc_bk1999()`: the dyad value is now 0 when the two firms meet in only one
  market, as in Baum & Korn (1999).
* "market firm" level: values are now taken from the same date. Before, with
  several dates, a firm could get the market value of another date.
* Weighted input (`mmc_count()`, `mmc_ekcount()`, `mmc_bgsm()`, `mmc_bk()`):
  `N_m` (firms in a market) and `N_i` (markets of a firm) are now counts of
  values greater than 0, not sums of the weights. Before, a weighted market
  could get negative or infinite values.
* `mmc_feinberg()` market level: the denominator is the total sales of the
  firms present in the market (according to `data`).
* `matrix_m` is matched to `data` by firm and date, not by row order.
* `mmc_count()` market levels work with a date column that is not called
  `year`.
* Unchanged for presence dummies: count, Evans & Kessides, Boeker et al.,
  Baum & Korn (1996) and Feinberg at the dyad, dyad-market and firm-market
  levels, and count, Evans & Kessides, Boeker et al. and Baum & Korn (1996) at
  the market level.

## Interface

* New `format` argument. The default `"long"` returns one row per unit with
  contacts (pair, pair and market, firm and market, or market). `"wide"`
  returns the earlier layout with one column per market.
* New `focal` argument: compute results for some firms while all firms in
  the data still count as rivals.
* New `avg_over` argument for the `avg_` columns: `"nonzero"` (default, as
  before), `"rivals"` or `"present"`.
* `mmc_bk()` gains `entry`: with `entry = TRUE`, the "firm market" level
  also returns Baum & Korn's (1996) value for markets the firm is not in
  (computed from its multimarket rivals there, for studies of entry), marked
  by the column `present`.
* `mmc_bk1999()` gains `centrality`: `"share"` (default) uses each firm's
  share of its own total in `matrix_m` as `C_in`; `"given"` uses the values
  in `matrix_m` directly, e.g. a centrality measure you computed yourself.
* Wide column names use underscores: `mmc_im_<market>`, `sum_mmc_im`,
  `<firm_col>_y`, and so on.
* Removed `write_to_disk`, `chunk_size` and `save_path`/`saveto`; they are no
  longer needed.
* NA in the market columns is now an error unless `fill_na = TRUE`.
* `data` and `matrix_m` are no longer modified by reference.
