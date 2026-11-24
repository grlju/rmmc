# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`rmmc` is an R package (only dependency: `data.table`) that calculates **multimarket contact (MMC)** between firms: seven measures, each at five levels ("dyad", "dyad market", "firm market", "market", "market firm"). The formulas follow Gimeno & Jeong (2001), Appendix A (pp. 403–408). A copy of the paper is in the repo root (`Gimeno-2001-Multimarket contact_ Meaning.pdf`, not in git). It is the reference for every formula. Optional variants: `mmc_bk(entry = TRUE)` (Baum & Korn 1996 for markets the firm is not in, "firm market" level only) and `mmc_bk1999(centrality = "share" | "given")`. Measure-specific options reach the engine as `opts`, which is stored in `inp$opts`.

## Commands

R 4.6, Rtools, devtools, roxygen2 and testthat are installed. Run from the repo root:

```powershell
Rscript -e "devtools::document()"                  # regenerate man/*.Rd and NAMESPACE (never edit those by hand)
Rscript -e "devtools::test()"                      # all tests
Rscript -e "devtools::test(filter = 'reference')"  # one file: tests/testthat/test-reference.R
Rscript -e "devtools::check(args = '--no-manual')" # full R CMD check (currently 0 errors/warnings/notes)
```

## Architecture

There is one shared engine, and each measure is a short definition:

- `R/input.R`: validates arguments and melts wide input (one row per firm and date, one column per market) into long tables. `P` holds (date, id, market, x) for x > 0. `S` holds (date, id, market, s) from `matrix_m`, joined by firm and date. Internal column names are always `date`, `id`, `id_y` and `market`. `.mmc_finish()` renames them to the user's `firm_col`/`date_col` at the end.
- `R/pairs.R`: the sparse pair table. `P` is self-joined on (date, market), giving one row per (date, i, j, m) where both firms are present, together with `M_ij` (shared-market count) and `S_ij` (Σ x_i·x_j). Rivals `j` are always all firms; `focal` restricts only `i`.
- `R/measures.R`: `.mmc_measures` is a list of specs. Each spec has a `pair()` function that adds `mmc_ij`/`mmc_ijm` to the pair table, an `im` rule (`"mean"` = ÷ rivals, `"bk"` = multimarket rivals only, `"sum"` = Feinberg), and an `m` rule (`"mean"` = ÷ N_m, `"sales"` = ÷ Σ S_im). **Adding or fixing a measure normally touches only this file.**
- `R/levels.R`: `.mmc_run()` is the single entry point. It aggregates to the requested level, computes the `sum_`/`avg_` summaries (`avg_over`), and builds long or wide output.
- `R/mmc_*.R`: exported wrappers containing only roxygen docs and a call to `.mmc_run()`. Shared parameter and return docs live in `mmc_count.R`; the other files use `@inheritParams`/`@inherit`.

Conventions that hold throughout:
- `N_m` and `N_i` are always **counts** of values > 0, also for weighted input.
- Markets without rivals produce no pair rows. Their firm-in-market value is 0, never NaN or Inf. Pitfalls the tests guard against: dividing by `N_m - 2` instead of the number of rivals, and using sums of weights as counts.
- For `bk1999`, `chen` and `feinberg`, `data` is presence only (x is set to 1) and the weights come from `matrix_m`.
- "market" and "market firm" levels always compute every firm's firm-in-market value, because a market value needs every firm in the market.

## Tests

`tests/testthat/helper-reference.R` is a deliberately naive, loop-based implementation written directly from the paper's formulas. It shares no code with the package. `test-reference.R` compares the engine with it on random data (dummies and weights, several dates, every measure × level × `avg_over`), and also checks wide against long output, `focal`, and runs without a date. `test-known-values.R` holds hand-computed values (toy examples with dummies and weights, plus `rmmc:::data` for year 2000). When changing a formula, change the reference independently and check that both agree.

Test data: `R/sysdata.rda` contains `rmmc:::data` (4 firms × 4 years, 0/1 columns `m1`–`m4`, columns `year`, `id`) and `rmmc:::m` (the same shape, with revenue values).
