context("Test chen")

test = rmmc:::data
test_m = rmmc:::m

test_that("Test chen with dataset with [date, dyad]", {
  mmc_chen(test, test_m, "id", "year", "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [date, dyad market]", {
  mmc_chen(test, test_m, "id", "year", "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [date, firm market]", {
  mmc_chen(test, test_m, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [date, market]", {
  mmc_chen(test, test_m, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [date, market firm]", {
  mmc_chen(test, test_m, "id", "year", "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [date, dyad, full]", {
  mmc_chen(test, test_m, "id", "year", "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [date, dyad market, full]", {
  mmc_chen(test, test_m, "id", "year", "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [date, firm market, full]", {
  mmc_chen(test, test_m, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [date, market, full]", {
  mmc_chen(test, test_m, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [date ,market firm, full]", {
  mmc_chen(test, test_m, "id", "year", "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

data2000 = data[year == 2000]
data2000[, year := NULL]
test = data2000
test_m = m[year == 2000]
test_m[, year := NULL]

test_that("Test chen with dataset with [dyad]", {
  mmc_chen(test, test_m, "id", NA, "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [dyad market]", {
  mmc_chen(test, test_m, "id", NA, "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [firm market]", {
  mmc_chen(test, test_m, "id", NA, "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [market]", {
  mmc_chen(test, test_m, "id", NA, "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [market firm]", {
  mmc_chen(test, test_m, "id", NA, "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test chen with dataset with [dyad, full]", {
  mmc_chen(test, test_m, "id", NA, "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [dyad market, full]", {
  mmc_chen(test, test_m, "id", NA, "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [firm market, full]", {
  mmc_chen(test, test_m, "id", NA, "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [market, full]", {
  mmc_chen(test, test_m, "id", NA, "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test chen with dataset with [market firm, full]", {
  mmc_chen(test, test_m, "id", NA, "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})
