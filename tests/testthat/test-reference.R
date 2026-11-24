# The sparse engine against the dense reference (helper-reference.R) on random
# data: every measure, level and averaging rule, with dummies and weights.

measures <- c("count", "ekcount", "bgsm", "bk", "bk1999", "chen", "feinberg")
weighted_ok <- c("count", "ekcount", "bgsm", "bk")
levels_all <- c("dyad", "dyad market", "firm market", "market", "market firm")

value_col <- c("dyad" = "mmc_ij", "dyad market" = "mmc_ijm", "firm market" = "mmc_im",
               "market" = "mmc_m", "market firm" = "mmc_mi")
key_cols <- list("dyad" = c("date", "id", "id_y"), "dyad market" = c("date", "id", "id_y", "market"),
                 "firm market" = c("date", "id", "market"), "market" = c("date", "market"),
                 "market firm" = c("date", "id", "market"))

compare_long <- function(measure, level, td, avg_over, ...) {
  got <- run_mmc(measure, td$data, td$sales, "id", "date", level, td$markets, avg_over = avg_over, ...)
  exp <- ref_long(measure, level, td$data, td$sales, td$markets, avg_over, ...)
  keys <- key_cols[[level]]
  v <- value_col[[level]]
  cols <- c(keys, v, if (level != "dyad") paste0(c("sum_", "avg_"), v))
  got <- got[, cols, with = FALSE]
  setnames(exp, c("value", if (level != "dyad") c("sum", "avg")), cols[-seq_along(keys)])
  setkeyv(got, keys)
  setkeyv(exp, keys)
  expect_equal(got, exp, tolerance = 1e-10, ignore_attr = TRUE,
               label = paste(measure, level, avg_over))
}

test_that("all measures match the reference at every level (dummies)", {
  for (seed in 1:3) {
    td <- random_data(seed)
    for (measure in measures) for (level in levels_all) {
      compare_long(measure, level, td, "nonzero")
    }
  }
})

test_that("averaging rules match the reference", {
  td <- random_data(11, n = 8, M = 6, D = 2)
  for (measure in measures) for (level in setdiff(levels_all, "dyad")) {
    for (avg_over in c("rivals", "present")) compare_long(measure, level, td, avg_over)
  }
})

test_that("weighted input matches the reference", {
  for (seed in 4:5) {
    td <- random_data(seed, weights = TRUE)
    for (measure in weighted_ok) for (level in levels_all) {
      compare_long(measure, level, td, "nonzero")
    }
  }
})

test_that("wide output holds the same values as long output, with 0 elsewhere", {
  td <- random_data(21)
  all_pairs <- td$data[, .N, by = date][, sum(N * (N - 1))]
  for (measure in measures) for (level in setdiff(levels_all, "dyad")) {
    v <- value_col[[level]]
    long <- run_mmc(measure, td$data, td$sales, "id", "date", level, td$markets)
    wide <- run_mmc(measure, td$data, td$sales, "id", "date", level, td$markets, format = "wide")
    keys <- setdiff(key_cols[[level]], "market")
    melted <- melt(wide, id.vars = keys, measure.vars = paste0(v, "_", td$markets),
                   variable.name = "market", value.name = "wide_value", variable.factor = FALSE)
    melted[, market := sub(paste0("^", v, "_"), "", market)]
    both <- long[melted, on = c(keys, "market")]
    expect_equal(both$wide_value, ifelse(is.na(both[[v]]), 0, both[[v]]), label = paste(measure, level))
    if (level %in% c("firm market", "market firm")) expect_equal(nrow(wide), nrow(td$data))
    if (level == "market") expect_equal(nrow(wide), uniqueN(td$data$date))
    if (level == "dyad market") expect_equal(nrow(wide), all_pairs)
  }
  wide_dyad <- mmc_count(td$data, "id", "date", "dyad", td$markets, format = "wide")
  expect_equal(nrow(wide_dyad), all_pairs)
  long_dyad <- mmc_count(td$data, "id", "date", "dyad", td$markets)
  expect_equal(sum(wide_dyad$mmc_ij), sum(long_dyad$mmc_ij))
})

test_that("focal firms get the same values as in the full computation", {
  td <- random_data(31, n = 9)
  focal <- c("f02", "f05", "f07")
  for (measure in measures) for (level in c("dyad", "dyad market", "firm market", "market firm")) {
    full <- run_mmc(measure, td$data, td$sales, "id", "date", level, td$markets)
    part <- run_mmc(measure, td$data, td$sales, "id", "date", level, td$markets, focal = focal)
    expect_equal(part, full[id %in% focal], ignore_attr = TRUE, label = paste(measure, level))
  }
  expect_warning(mmc_count(td$data, "id", "date", "market", td$markets, focal = "f01"), "ignored")
})

test_that("results without a date column equal those of a single date", {
  td <- random_data(41, D = 1)
  nodate <- copy(td$data)[, date := NULL]
  nodate_sales <- copy(td$sales)[, date := NULL]
  for (measure in measures) for (level in levels_all) {
    a <- run_mmc(measure, td$data, td$sales, "id", "date", level, td$markets)
    b <- run_mmc(measure, nodate, nodate_sales, "id", NA, level, td$markets)
    expect_equal(b, a[, -"date"], ignore_attr = TRUE, label = paste(measure, level))
  }
})

test_that("Baum & Korn 1996 entry values match the reference", {
  for (seed in 51:53) for (weights in c(FALSE, TRUE)) {
    td <- random_data(seed, n = 8, M = 6, D = 2, weights = weights)
    for (avg_over in c("nonzero", "rivals", "present")) {
      compare_long("bk", "firm market", td, avg_over, entry = TRUE)
    }
  }
  td <- random_data(54)
  plain <- mmc_bk(td$data, "id", "date", "firm market", td$markets)
  ent <- mmc_bk(td$data, "id", "date", "firm market", td$markets, entry = TRUE)
  expect_equal(ent[present == TRUE, -"present"], plain, ignore_attr = TRUE)
  expect_true(any(!ent$present))
  expect_error(mmc_bk(td$data, "id", "date", "market", td$markets, entry = TRUE), "firm market")
  wide <- mmc_bk(td$data, "id", "date", "firm market", td$markets, entry = TRUE, format = "wide")
  expect_equal(nrow(wide), nrow(td$data))
})

test_that("Baum & Korn 1999 with centrality given matches the reference", {
  td <- random_data(61)
  for (level in levels_all) compare_long("bk1999", level, td, "nonzero", centrality = "given")
  expect_false(isTRUE(all.equal(
    mmc_bk1999(td$data, td$sales, "id", "date", "dyad", td$markets, centrality = "given"),
    mmc_bk1999(td$data, td$sales, "id", "date", "dyad", td$markets))))
})