#' @title mmc_bk1999
#' @description Calculates multimarket contact with Baum & Korn's (1999)
#'   measure: the centrality-weighted proportion of jointly occupied markets.
#'
#' @inheritParams mmc_count
#' @param matrix_m A data.table or data.frame with the same firm, date and
#'   market columns as \code{data}, holding the firms' values (e.g. revenue or
#'   assets) in each market. It is matched to \code{data} by firm and date, so
#'   its row order does not matter. Every firm (and date) in \code{data} needs
#'   a row.
#' @inherit mmc_count return references
#'
#' @details
#' \deqn{MMC_{ij} = \frac{\sum_n (C_{in} + C_{jn}) I_{in} I_{jn}}{\sum_n I_{in} + \sum_n I_{jn}}}{MMC_ij = sum_n (C_in + C_jn) I_in I_jn / (sum_n I_in + sum_n I_jn)}
#' if the firms meet in more than one market (\eqn{M_{ij} > 1}), else 0.
#' \deqn{MMC_{ijm} = I_{im} I_{jm} MMC_{ij}}{MMC_ijm = I_im I_jm MMC_ij}
#' \deqn{MMC_{im} = \sum_{j \ne i} MMC_{ijm} / (N_m - 1)}{MMC_im = sum_{j != i} MMC_ijm / (N_m - 1)}
#' \deqn{MMC_m = \sum_i MMC_{im} / N_m}{MMC_m = sum_i MMC_im / N_m}
#'
#' \eqn{C_{in}} is the centrality of market \eqn{n} for firm \eqn{i}. Baum &
#' Korn (1999) used the proportion of an airline's routes connecting with a
#' route. With \code{centrality = "given"}, \code{matrix_m} holds
#' \eqn{C_{in}} directly (e.g. a centrality you computed). With
#' \code{centrality = "share"} (default), \eqn{C_{in}} is firm \eqn{i}'s share
#' of its own total in \code{matrix_m}: \eqn{C_{in} = s_{in} / \sum_n s_{in}},
#' e.g. the share of its revenue or assets in market \eqn{n}. Presence
#' (\eqn{I_{in}}) comes from \code{data}.
#'
#' @param centrality "share" (default): \eqn{C_{in}} is each firm's share of
#'   its own total in \code{matrix_m}. "given": the values in \code{matrix_m}
#'   are used as \eqn{C_{in}} as they are.
#'
#' @export
#'
#' @examples
#' data = rmmc:::data
#' m = rmmc:::m
#' mmc_bk1999(data, m, "id", "year", "dyad", c("m1", "m2", "m3", "m4"))
mmc_bk1999 = function(data, matrix_m, firm_col, date_col = NA, level, market_cols = NA,
                      subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                      focal = NULL, format = c("long", "wide"),
                      avg_over = c("nonzero", "rivals", "present"),
                      centrality = c("share", "given")) {
  centrality <- match.arg(centrality)
  .mmc_run("bk1999", data, matrix_m, firm_col, date_col, level, market_cols, subset_row,
           fill_na, full_result, focal, format, avg_over, opts = list(centrality = centrality))
}
