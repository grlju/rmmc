#' @title mmc_bgsm
#' @description Calculates multimarket contact with Boeker, Goodstein, Stephan
#'   & Murmann's (1997) measure: the average share of the focal firm's markets
#'   in which it meets its rivals in the focal market.
#'
#' @inheritParams mmc_count
#' @inherit mmc_count return references
#'
#' @details
#' \deqn{MMC_{ij} = \sum_n I_{in} I_{jn} / \sum_n I_{in}}{MMC_ij = sum_n I_in I_jn / sum_n I_in}
#' \deqn{MMC_{ijm} = I_{im} I_{jm} MMC_{ij}}{MMC_ijm = I_im I_jm MMC_ij}
#' \deqn{MMC_{im} = \sum_{j \ne i} MMC_{ijm} / (N_m - 1)}{MMC_im = sum_{j != i} MMC_ijm / (N_m - 1)}
#' \deqn{MMC_m = \sum_i MMC_{im} / N_m}{MMC_m = sum_i MMC_im / N_m}
#' The measure is asymmetric (\eqn{MMC_{ij} \ne MMC_{ji}}) and includes the
#' contact in the focal market.
#'
#' @export
#'
#' @examples
#' data = rmmc:::data
#' mmc_bgsm(data, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"))
mmc_bgsm = function(data, firm_col, date_col = NA, level, market_cols = NA,
                    subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                    focal = NULL, format = c("long", "wide"),
                    avg_over = c("nonzero", "rivals", "present")) {
  .mmc_run("bgsm", data, NULL, firm_col, date_col, level, market_cols, subset_row,
           fill_na, full_result, focal, format, avg_over)
}
