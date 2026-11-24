#' @title mmc_bk
#' @description Calculates multimarket contact with Baum & Korn's (1996)
#'   measure: the average share of the focal firm's markets in which it meets
#'   its multimarket rivals in the focal market.
#'
#' @inheritParams mmc_count
#' @inherit mmc_count return references
#'
#' @details
#' \deqn{MMC_{ij} = \sum_n I_{in} I_{jn} / \sum_n I_{in}}{MMC_ij = sum_n I_in I_jn / sum_n I_in}
#' \deqn{MMC_{ijm} = I_{im} I_{jm} MMC_{ij}}{MMC_ijm = I_im I_jm MMC_ij}
#' \deqn{MMC_{im} = \sum_{j \ne i} B_{ij} MMC_{ijm} / \sum_{j \ne i} B_{ij} I_{jm}}{MMC_im = sum_{j != i} B_ij MMC_ijm / sum_{j != i} B_ij I_jm}
#' \deqn{MMC_m = \sum_i MMC_{im} / N_m}{MMC_m = sum_i MMC_im / N_m}
#' where \eqn{B_{ij} = 1} if firms \eqn{i} and \eqn{j} meet in more than one
#' market (\eqn{M_{ij} > 1}), else 0. Single-market rivals therefore do not
#' affect the firm-in-market value, which is 0 when the firm has no
#' multimarket rival in the market. The dyad and dyad-market levels equal
#' those of \code{\link{mmc_bgsm}}.
#'
#' Gimeno & Jeong (2001: 374) note that Baum & Korn's firm-in-market measure
#' does not require the focal firm to be present in the focal market (it was
#' used to study entry). By default, as in their Appendix A
#' (\eqn{MMC_{ijm} = I_{im} I_{jm} MMC_{ij}}), it is computed only for
#' markets the firm is present in. With \code{entry = TRUE}, the "firm
#' market" level also returns markets the firm is not in, using the
#' paper's text version
#' \deqn{MMC_{im} = \frac{\sum_{j \ne i} B_{ij} I_{jm} M_{ij} / \sum_n I_{in}}{\sum_{j \ne i} B_{ij} I_{jm}}}{MMC_im = sum_{j != i} B_ij I_jm (M_ij / sum_n I_in) / sum_{j != i} B_ij I_jm}
#' (with weights, \eqn{I_{jm}} in the numerator is the rival's weight
#' \eqn{x_{jm}}). A row is returned for every market in which the firm has
#' at least one multimarket rival. The column \code{present} tells the two
#' kinds of rows apart. The firm's \code{sum_mmc_im} and \code{avg_mmc_im}
#' still summarise only the markets it is present in.
#'
#' @param entry Logical. If \code{TRUE}, also compute the firm-in-market
#'   value for markets the firm is not present in (for studies of entry).
#'   Only at the "firm market" level.
#'
#' @export
#'
#' @examples
#' data = rmmc:::data
#' mmc_bk(data, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"))
#' mmc_bk(data, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"),
#'        entry = TRUE)
mmc_bk = function(data, firm_col, date_col = NA, level, market_cols = NA,
                  subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                  focal = NULL, format = c("long", "wide"),
                  avg_over = c("nonzero", "rivals", "present"), entry = FALSE) {
  .mmc_run("bk", data, NULL, firm_col, date_col, level, market_cols, subset_row,
           fill_na, full_result, focal, format, avg_over, opts = list(entry = entry))
}
