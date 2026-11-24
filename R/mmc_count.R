#' @title mmc_count
#' @description Calculates multimarket contact as the count of contacts outside
#'   the focal market (Gimeno & Jeong, 2001).
#'
#' @param data A data.table or data.frame (or an object coercible to a
#'   data.table) with one row per firm (and date) and one column per market.
#'   Values greater than 0 mean the firm is present in the market. For
#'   \code{mmc_count}, \code{mmc_ekcount}, \code{mmc_bgsm} and \code{mmc_bk} the
#'   values may be weights (e.g. the share of a firm's deposits in each market);
#'   the paper's dummies \eqn{I_{im}} are then replaced by the weights. For the
#'   measures that take \code{matrix_m}, \code{data} only indicates presence.
#'   \code{data} is not modified.
#' @param firm_col A string with the name of the column of firm identifiers.
#' @param date_col A string with the name of the column of dates, or \code{NA}.
#'   If provided, the measures are computed for each date separately.
#' @param level A string: "dyad", "dyad market", "firm market", "market" or
#'   "market firm". See the Value section.
#' @param market_cols A character vector with the names of the market columns.
#' @param subset_row \code{FALSE}, or a logical vector with one element per row
#'   of \code{data}. Rows that are not \code{TRUE} are dropped before anything
#'   is computed, so the dropped firms do not count as rivals either. To keep
#'   all rivals but report only some firms, use \code{focal}.
#' @param fill_na Logical. If \code{TRUE}, NA in the market columns is treated
#'   as 0. If \code{FALSE} (the default), NA is an error.
#' @param full_result Logical. If \code{TRUE}, intermediate columns are kept
#'   (only with \code{format = "long"}).
#' @param focal \code{NULL} (all firms), or a vector of firm identifiers for
#'   which results are returned. All firms in \code{data} still count as
#'   rivals, so the values for the focal firms are the same as without
#'   \code{focal}; only the work and the output shrink. Ignored at the
#'   "market" level.
#' @param format "long" (default) or "wide". See the Value section.
#' @param avg_over Which markets the \code{avg_} columns average over:
#'   \describe{
#'   \item{"nonzero"}{(default) markets with a non-zero value. This is how
#'     earlier versions of the package averaged.}
#'   \item{"rivals"}{markets where the measure has at least one rival
#'     (for \code{mmc_bk}: one multimarket rival), including those whose value
#'     is 0.}
#'   \item{"present"}{every market of the unit (the firm's \eqn{N_i} markets,
#'     a pair's shared markets, or every market with a firm). Gimeno & Jeong
#'     (2001) do not define this average; "present" is the analogue of the
#'     way they aggregate to the market level, where firms with a value of 0
#'     are included.}
#'   }
#'
#' @return A data.table. Column names follow \code{firm_col} and
#'   \code{date_col}; the rival's identifier is \code{<firm_col>_y}. The date
#'   column is omitted when \code{date_col = NA}. \eqn{N_i} is the number of
#'   markets a firm is present in and \eqn{N_m} the number of firms present in
#'   a market (both are counts, also for weighted input).
#'
#' With \code{format = "long"} (default), only units with contacts are
#' returned:
#' \describe{
#' \item{"dyad"}{One row per pair of firms that share at least one market:
#'   date, firm, \code{N_i}, rival, \code{N_j}, \code{mmc_ij}.}
#' \item{"dyad market"}{One row per pair and shared market: date, firm,
#'   \code{N_i}, rival, \code{N_j}, market, \code{mmc_ijm}, and the pair's
#'   \code{sum_mmc_ijm} and \code{avg_mmc_ijm} over its shared markets.}
#' \item{"firm market"}{One row per focal firm and market it is present in:
#'   date, firm, \code{N_i}, market, \code{mmc_im}, and the firm's
#'   \code{sum_mmc_im} and \code{avg_mmc_im} over its markets. Markets where
#'   the firm has no rival have \code{mmc_im = 0}.}
#' \item{"market"}{One row per market with at least one firm: date, market,
#'   \code{N_m}, \code{mmc_m}, and \code{sum_mmc_m} and \code{avg_mmc_m} over
#'   the markets at that date.}
#' \item{"market firm"}{One row per focal firm and market it is present in,
#'   with the market's value \code{mmc_mi} (= \code{mmc_m}): date, firm,
#'   \code{N_i}, market, \code{mmc_mi}, \code{sum_mmc_mi}, \code{avg_mmc_mi}.}
#' }
#'
#' With \code{format = "wide"}, every firm (or pair of firms, or date) is
#' returned, with one column per market named \code{mmc_ijm_<market>},
#' \code{mmc_im_<market>}, \code{mmc_m_<market>} or \code{mmc_mi_<market>}
#' and 0 where there is no contact. At the "dyad" level, every ordered pair
#' of firms is returned. Wide output needs memory for every firm (pair) times
#' every market.
#'
#' With \code{full_result = TRUE} (long format), intermediate columns are
#' added, for example \code{M_ij} (number of shared markets), \code{S_ij}
#' (sum of \eqn{x_{im} x_{jm}} over shared markets), \code{n_rivals},
#' \code{num} and \code{den} (numerator and denominator of the firm-in-market
#' or market value).
#'
#' @details
#' With \eqn{I_{im} = 1} if firm \eqn{i} is present in market \eqn{m}:
#' \deqn{M_{ij} = \sum_n I_{in} I_{jn}}{M_ij = sum_n I_in I_jn}
#' \deqn{M_{ijm} = I_{im} I_{jm} \sum_{n \ne m} I_{in} I_{jn}}{M_ijm = I_im I_jm sum_{n != m} I_in I_jn}
#' \deqn{MMC_{im} = \sum_{j \ne i} M_{ijm} / (N_m - 1)}{MMC_im = sum_{j != i} M_ijm / (N_m - 1)}
#' \deqn{MMC_m = \sum_i MMC_{im} / N_m}{MMC_m = sum_i MMC_im / N_m}
#' The dyad level returns \eqn{M_{ij}}, the dyad-market level \eqn{M_{ijm}}.
#'
#' @references Gimeno, J. and Jeong, E. (2001), "Multimarket contact: meaning
#'   and measurement at multiple levels of analysis", in Baum, J.A.C. and
#'   Greve, H.R. (Ed.) Multiunit Organization and Multimarket Strategy
#'   (Advances in Strategic Management, Vol. 18), Emerald, pp. 357-408.
#'   \doi{10.1016/S0742-3322(01)18013-7}
#'
#' @export
#'
#' @examples
#' data = rmmc:::data
#' mmc_count(data, "id", "year", "firm market", market_cols = c("m1", "m2", "m3", "m4"))
#' mmc_count(data, "id", "year", "market", market_cols = c("m1", "m2", "m3", "m4"),
#'           format = "wide")
mmc_count = function(data, firm_col, date_col = NA, level, market_cols = NA,
                     subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                     focal = NULL, format = c("long", "wide"),
                     avg_over = c("nonzero", "rivals", "present")) {
  .mmc_run("count", data, NULL, firm_col, date_col, level, market_cols, subset_row,
           fill_na, full_result, focal, format, avg_over)
}
