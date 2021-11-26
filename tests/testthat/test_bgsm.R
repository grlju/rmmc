context("Test bgsm")

test = rmmc:::data

test_that("Test bgsm with dataset with [date, dyad]", {
  mmc_bgsm(test, "id", "year", "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [date, dyad market]", {
  mmc_bgsm(test, "id", "year", "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [date, firm market]", {
  mmc_bgsm(test, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [date, market]", {
  mmc_bgsm(test, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [date, market firm]", {
  mmc_bgsm(test, "id", "year", "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [date, dyad, full]", {
  mmc_bgsm(test, "id", "year", "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [date, dyad market, full]", {
  mmc_bgsm(test, "id", "year", "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [date, firm market, full]", {
  mmc_bgsm(test, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [date, market, full]", {
  mmc_bgsm(test, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [date ,market firm, full]", {
  mmc_bgsm(test, "id", "year", "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test = test[year == 2000]
test[, year := NULL]

test_that("Test bgsm with dataset with [dyad]", {
  mmc_bgsm(test, "id", NA, "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [dyad market]", {
  mmc_bgsm(test, "id", NA, "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [firm market]", {
  mmc_bgsm(test, "id", NA, "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [market]", {
  mmc_bgsm(test, "id", NA, "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [market firm]", {
  mmc_bgsm(test, "id", NA, "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE)
})

test_that("Test bgsm with dataset with [dyad, full]", {
  mmc_bgsm(test, "id", NA, "dyad", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [dyad market, full]", {
  mmc_bgsm(test, "id", NA, "dyad market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [firm market, full]", {
  mmc_bgsm(test, "id", NA, "firm market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [market, full]", {
  mmc_bgsm(test, "id", NA, "market", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})

test_that("Test bgsm with dataset with [market firm, full]", {
  mmc_bgsm(test, "id", NA, "market firm", market_cols = c("m1", "m2", "m3", "m4"), fill_na = FALSE, full_result = TRUE)
})
