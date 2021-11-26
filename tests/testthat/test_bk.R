context("Test bk")

test = rmmc:::data

test_that("Test bk with dataset with [date, dyad]", {
  mmc_bk(test, "id", "year", "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [date, dyad market]", {
  mmc_bk(test, "id", "year", "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [date, firm market]", {
  mmc_bk(test, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [date, market]", {
  mmc_bk(test, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [date, market firm]", {
  mmc_bk(test, "id", "year", "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [date, dyad, full]", {
  mmc_bk(test, "id", "year", "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [date, dyad market, full]", {
  mmc_bk(test, "id", "year", "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [date, firm market, full]", {
  mmc_bk(test, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [date, market, full]", {
  mmc_bk(test, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [date ,market firm, full]", {
  mmc_bk(test, "id", "year", "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

data2000 = data[year == 2000]
data2000[, year := NULL]
test = data2000

test_that("Test bk with dataset with [dyad]", {
  mmc_bk(test, "id", NA, "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [dyad market]", {
  mmc_bk(test, "id", NA, "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [firm market]", {
  mmc_bk(test, "id", NA, "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [market]", {
  mmc_bk(test, "id", NA, "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [market firm]", {
  mmc_bk(test, "id", NA, "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bk with dataset with [dyad, full]", {
  mmc_bk(test, "id", NA, "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [dyad market, full]", {
  mmc_bk(test, "id", NA, "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [firm market, full]", {
  mmc_bk(test, "id", NA, "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [market, full]", {
  mmc_bk(test, "id", NA, "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bk with dataset with [market firm, full]", {
  mmc_bk(test, "id", NA, "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})
