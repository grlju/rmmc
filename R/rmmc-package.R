#' rmmc: Multimarket Contact Measures
#'
#' Calculates seven measures of multimarket contact at five levels of analysis
#' (dyad, dyad in market, firm in market, market, and market firm), following
#' the definitions in Gimeno, J. and Jeong, E. (2001), "Multimarket contact:
#' meaning and measurement at multiple levels of analysis", Advances in
#' Strategic Management 18, 357-408, \doi{10.1016/S0742-3322(01)18013-7}.
#'
#' All measures share one sparse computation: firms are paired only within the
#' markets they share, so the cost grows with the number of actual contacts,
#' not with the square of the number of firms times the number of markets.
#'
#' @section Code layout:
#' \itemize{
#' \item \code{R/input.R}: argument checks and conversion of the wide input to
#'   long presence and sales tables.
#' \item \code{R/pairs.R}: the sparse pair table (one row per date, focal firm,
#'   rival and shared market).
#' \item \code{R/measures.R}: the definition of each measure (pair-level terms
#'   and how they are aggregated).
#' \item \code{R/levels.R}: aggregation to the five levels, firm and market
#'   summaries, and long or wide output.
#' \item \code{R/mmc_*.R}: the exported functions, which only document the
#'   measure and call the shared code.
#' }
#'
#' @keywords internal
#' @import data.table
#' @importFrom utils globalVariables
"_PACKAGE"

# Column names used in data.table expressions.
globalVariables(c(
  "B", "C_i", "C_j", "IxI", "M_ij", "N_i", "N_j", "N_m", "P_i", "P_n",
  "S_ij", "W_i", "date", "den", "has_rival", "i.N_i", "i.N_m", "i.den", "id",
  "id_y", "market", "mmc_ij", "mmc_ijm", "mmc_im", "mmc_m", "num", "present", "s",
  "s_i", "s_j", "v", "x", "x.N_m", "x.has_rival", "x.mmc_m", "x_i", "x_j"
))
