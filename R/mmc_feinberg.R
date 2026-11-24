#' @title mmc_feinberg
#' @description Calculates multimarket contact with Feinberg's (1985)
#'   "sales at risk": the focal firm's sales in the other markets where it
#'   meets its focal-market rivals.
#'
#' @inheritParams mmc_bk1999
#' @inherit mmc_count return references
#'
#' @details
#' \deqn{SAR_{ij} = \sum_n I_{in} I_{jn} S_{in}}{SAR_ij = sum_n I_in I_jn S_in}
#' \deqn{SAR_{ijm} = I_{im} I_{jm} \sum_{n \ne m} I_{in} I_{jn} S_{in}}{SAR_ijm = I_im I_jm sum_{n != m} I_in I_jn S_in}
#' \deqn{SAR_{im} = \sum_{j \ne i} SAR_{ijm}}{SAR_im = sum_{j != i} SAR_ijm}
#' \deqn{SAR_m = \sum_i SAR_{im} / \sum_i S_{im}}{SAR_m = sum_i SAR_im / sum_i S_im}
#' where \eqn{S_{in}} is firm \eqn{i}'s sales in market \eqn{n} from
#' \code{matrix_m}. Unlike the other measures, the firm-in-market value is a
#' sum over rivals, not an average, so it grows with the number of rivals.
#' The market value divides by the total sales of the firms in the market.
#'
#' @export
#'
#' @examples
#' data = rmmc:::data
#' m = rmmc:::m
#' mmc_feinberg(data, m, "id", "year", "firm market", c("m1", "m2", "m3", "m4"))
mmc_feinberg = function(data, matrix_m, firm_col, date_col = NA, level, market_cols = NA,
                        subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                        focal = NULL, format = c("long", "wide"),
                        avg_over = c("nonzero", "rivals", "present")) {
  .mmc_run("feinberg", data, matrix_m, firm_col, date_col, level, market_cols, subset_row,
           fill_na, full_result, focal, format, avg_over)
}
