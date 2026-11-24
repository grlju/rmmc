# Hand-checked values: small toy examples (dummies and weights) and the package data.

toy <- data.table(
  id = c("1", "2", "3"),
  m1 = c(1, 1, 1),
  m2 = c(1, 1, 0),
  m3 = c(1, 0, 1)
)
mk3 <- c("m1", "m2", "m3")

test_that("count and BGSM firm-in-market values of the toy example", {
  cnt <- unique(mmc_count(toy, "id", NA, "firm market", mk3)[, .(id, sum_mmc_im, avg_mmc_im)])
  expect_equal(cnt$sum_mmc_im, c(3, 1.5, 1.5))
  expect_equal(cnt$avg_mmc_im, c(1, 0.75, 0.75))

  bg <- unique(mmc_bgsm(toy, "id", NA, "firm market", mk3)[, .(id, sum_mmc_im, avg_mmc_im)])
  expect_equal(bg$sum_mmc_im, c(2, 1.75, 1.75))
  expect_equal(bg$avg_mmc_im, c(2 / 3, 0.875, 0.875))
})

test_that("weighted toy example uses the number of rivals, not the sum of weights", {
  wtoy <- data.table(
    id = c("1", "2", "3"),
    m1 = c(0.5, 0.6, 0.9),
    m2 = c(0.3, 0.4, 0),
    m3 = c(0.2, 0, 0.1)
  )
  cnt <- unique(mmc_count(wtoy, "id", NA, "firm market", mk3)[, .(id, N_i, avg_mmc_im)])
  expect_equal(cnt$avg_mmc_im, c(0.0225, 0.027, 0.00675))
  expect_equal(cnt$N_i, c(3L, 2L, 2L))
})

test_that("package data, year 2000, matches Gimeno & Jeong's formulas", {
  d <- rmmc:::data[year == 2000]
  m <- rmmc:::m[year == 2000]
  mk <- c("m1", "m2", "m3", "m4")

  # Chen: firm1-firm3 share m2 and m3. P_n is the market total.
  chen <- mmc_chen(d, m, "id", "year", "dyad", mk)
  expect_equal(chen[id == "firm1" & id_y == "firm3", mmc_ij], 50 / 175 * 30 / 80 + 25 / 175 * 50 / 75)

  # Baum & Korn 1999: 0 for pairs that meet in one market only.
  bk99 <- mmc_bk1999(d, m, "id", "year", "dyad", mk)
  expect_equal(bk99[id == "firm1" & id_y == "firm4", mmc_ij], 0)
  expect_equal(bk99[id == "firm1" & id_y == "firm3", mmc_ij],
               ((50 / 175 + 30 / 160) + (25 / 175 + 50 / 160)) / 6)

  # Feinberg: market value is the sum of SAR_im over total sales in the market.
  fb <- mmc_feinberg(d, m, "id", "year", "market", mk)
  expect_equal(fb[market == "m2", mmc_m], (25 + 50) / (50 + 30))

  # BGSM market value is the mean over the market's firms.
  bg <- mmc_bgsm(d, "id", "year", "market", mk)
  expect_equal(bg$mmc_m, rep(2 / 3, 4))

  # Baum & Korn 1996 ignores single-market rivals.
  bk <- mmc_bk(d, "id", "year", "firm market", mk)
  expect_equal(bk[id == "firm1", mmc_im], c(0, 2 / 3, 2 / 3))
})

test_that("edge cases: no rival, one rival, single-market pairs, empty firms", {
  e <- data.table(
    id = c("a", "b", "c", "d"),
    m1 = c(1, 0, 0, 0),   # a alone
    m2 = c(1, 1, 0, 0),   # a and b: exactly one rival
    m3 = c(0, 1, 1, 0),   # b and c meet only here
    m4 = c(0, 0, 0, 0)    # empty market; d has no market
  )
  mk <- paste0("m", 1:4)
  fm <- mmc_count(e, "id", NA, "firm market", mk, full_result = TRUE)
  expect_equal(fm[id == "a" & market == "m1", c(mmc_im, n_rivals)], c(0, 0))
  expect_equal(fm[id == "a" & market == "m2", mmc_im], 0)   # pair meets once: term 0
  expect_false("d" %in% fm$id)
  expect_false(any(is.na(fm$mmc_im)) || any(!is.finite(fm$mmc_im)))

  wide <- mmc_count(e, "id", NA, "firm market", mk, format = "wide")
  expect_equal(wide[id == "d", c(N_i, mmc_im_m1, sum_mmc_im, avg_mmc_im)], c(0, 0, 0, 0))
  expect_true("mmc_im_m4" %in% names(wide))

  mkt <- mmc_count(e, "id", NA, "market", mk, avg_over = "present")
  expect_equal(mkt$market, c("m1", "m2", "m3"))
})
