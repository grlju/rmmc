#' @title mmc_chen
#' @description Calculates multimarket contact with Chen's (1996) market
#'   commonality: the rival's market share in the focal firm's markets,
#'   weighted by the importance of each market to the focal firm.
#'
#' @inheritParams mmc_bk1999
#' @inherit mmc_count return references
#'
#' @details
#' \deqn{MktComm_{ij} = \sum_n \frac{P_{in}}{P_i} \cdot \frac{P_{jn}}{P_n}}{MktComm_ij = sum_n (P_in / P_i) (P_jn / P_n)}
#' \deqn{MktComm_{ijm} = I_{im} I_{jm} MktComm_{ij}}{MktComm_ijm = I_im I_jm MktComm_ij}
#' \deqn{MktComm_{im} = \sum_{j \ne i} MktComm_{ijm} / (N_m - 1)}{MktComm_im = sum_{j != i} MktComm_ijm / (N_m - 1)}
#' \deqn{MktComm_m = \sum_i MktComm_{im} / N_m}{MktComm_m = sum_i MktComm_im / N_m}
#' where \eqn{P_{in}} is firm \eqn{i}'s value in market \eqn{n} from
#' \code{matrix_m} (Chen used passengers), \eqn{P_i} its total over all
#' markets, and \eqn{P_n} the total of market \eqn{n} over all firms in the
#' data at that date. The sum runs over the markets both firms are present in
#' (according to \code{data}). The measure is asymmetric.
#'
#' @export
#'
#' @examples
#' data = rmmc:::data
#' m = rmmc:::m
#' mmc_chen(data, m, "id", "year", "dyad", c("m1", "m2", "m3", "m4"))
mmc_chen = function(data, matrix_m, firm_col, date_col = NA, level, market_cols = NA,
                    subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                    focal = NULL, format = c("long", "wide"),
                    avg_over = c("nonzero", "rivals", "present")) {
  .mmc_run("chen", data, matrix_m, firm_col, date_col, level, market_cols, subset_row,
           fill_na, full_result, focal, format, avg_over)
}
