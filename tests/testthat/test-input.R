d <- rmmc:::data
m <- rmmc:::m
mk <- c("m1", "m2", "m3", "m4")

test_that("arguments are checked", {
  expect_error(mmc_count(d, NA, "year", "dyad", mk), "firm_col")
  expect_error(mmc_count(d, "id", "year", "dyads", mk), "not one of")
  expect_error(mmc_count(d, "id", "year", "dyad"), "markets")
  expect_error(mmc_count(d, "id", "year", "dyad", c(mk, "m9")), "missing these columns")
  expect_error(mmc_count(d, "id", NA, "dyad", mk), "not unique")
  expect_error(mmc_count(d, "id", "year", "dyad", mk, format = "tall"))
  expect_error(mmc_count(d, "id", "year", "dyad", mk, subset_row = c(TRUE, FALSE)), "subset_row")
  expect_error(mmc_chen(d, NULL, "id", "year", "dyad", mk), "matrix_m")
  expect_error(mmc_chen(d, m[-1], "id", "year", "dyad", mk), "no row")
  neg <- copy(d)[1, m1 := -1]
  expect_error(mmc_count(neg, "id", "year", "dyad", mk), "negative")
})

test_that("NA is an error unless fill_na = TRUE", {
  na <- copy(d)[1, m2 := NA]
  expect_error(mmc_count(na, "id", "year", "dyad", mk), "fill_na")
  zero <- copy(d)[1, m2 := 0]
  expect_equal(mmc_count(na, "id", "year", "dyad", mk, fill_na = TRUE),
               mmc_count(zero, "id", "year", "dyad", mk))
})

test_that("input data is not modified", {
  before <- copy(d)
  before_m <- copy(m)
  mmc_feinberg(d, m, "id", "year", "firm market", mk, fill_na = TRUE)
  mmc_count(as.data.frame(d), "id", "year", "dyad", mk)
  expect_identical(d, before)
  expect_identical(m, before_m)
})

test_that("matrix_m is matched by firm and date, not by row order", {
  shuffled <- m[sample(nrow(m))]
  expect_equal(mmc_chen(d, shuffled, "id", "year", "dyad market", mk),
               mmc_chen(d, m, "id", "year", "dyad market", mk))
})

test_that("subset_row drops firms as rivals, focal keeps them", {
  keep <- d$id != "firm4"
  sub <- mmc_count(d, "id", "year", "dyad", mk, subset_row = keep)
  expect_false("firm4" %in% c(sub$id, sub$id_y))
  foc <- mmc_count(d, "id", "year", "dyad", mk, focal = c("firm1", "firm2", "firm3"))
  expect_true("firm4" %in% foc$id_y)
  expect_warning(mmc_count(d, "id", "year", "dyad", mk, focal = c("firm1", "nope")), "not in the data")
})

test_that("output column names follow firm_col and date_col", {
  d2 <- copy(d)
  setnames(d2, c("year", "id"), c("t", "bank"))
  r <- mmc_count(d2, "bank", "t", "dyad", mk)
  expect_equal(names(r)[1:4], c("t", "bank", "N_i", "bank_y"))
  expect_warning(mmc_count(d, "id", "year", "dyad", mk, format = "wide", full_result = TRUE), "full_result")
})
