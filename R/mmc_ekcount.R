#' @title mmc_ekcount
#' @description Calculates multimarket contact with Evans & Kessides' (1994)
#'   count measure, which, unlike \code{\link{mmc_count}}, includes the contact
#'   in the focal market.
#'
#' @inheritParams mmc_count
#' @inherit mmc_count return references
#'
#' @details
#' \deqn{M_{ij} = \sum_n I_{in} I_{jn}}{M_ij = sum_n I_in I_jn}
#' \deqn{MMC_{ijm} = I_{im} I_{jm} M_{ij}}{MMC_ijm = I_im I_jm M_ij}
#' \deqn{MMC_{im} = \sum_{j \ne i} MMC_{ijm} / (N_m - 1)}{MMC_im = sum_{j != i} MMC_ijm / (N_m - 1)}
#' \deqn{MMC_m = \sum_i MMC_{im} / N_m}{MMC_m = sum_i MMC_im / N_m}
#' The market level is Evans & Kessides' original measure: the average count
#' over all dyads in the market, \eqn{\sum_i \sum_{j > i} I_{im} I_{jm} M_{ij} /
#' (N_m (N_m - 1) / 2)}, which equals \eqn{MMC_m} above.
#'
#' @export
#'
#' @examples
#' data = rmmc:::data
#' mmc_ekcount(data, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"))
mmc_ekcount = function(data, firm_col, date_col = NA, level, market_cols = NA,
                       subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                       focal = NULL, format = c("long", "wide"),
                       avg_over = c("nonzero", "rivals", "present")) {
  .mmc_run("ekcount", data, NULL, firm_col, date_col, level, market_cols, subset_row,
           fill_na, full_result, focal, format, avg_over)
}
