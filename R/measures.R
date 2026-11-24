# Definitions of the seven measures (Gimeno & Jeong 2001, Appendix A).
#
# Each measure is a list with
#   needs_m        TRUE if it uses matrix_m (sales, revenue, passengers...)
#   presence_only  TRUE if data is only used as a presence indicator (x > 0)
#   pair(pr, inp)  adds to the pair table, by reference,
#                    mmc_ij   the dyad-level value (constant within a pair)
#                    mmc_ijm  the dyad-in-market value for the row's market
#                  and any columns the aggregation needs (B for Baum & Korn 1996)
#   im             how the dyad-in-market values of i's rivals in m become the
#                  firm-in-market value:
#                    "mean" sum over rivals / number of rivals (N_m - 1)
#                    "bk"   sum over multimarket rivals / number of them
#                    "sum"  sum over rivals (Feinberg)
#   m              how firm-in-market values become the market value:
#                    "mean"  sum over firms in m / N_m
#                    "sales" sum over firms in m / total sales of those firms
#
# Notation: x_im is the value in data (presence dummy or weight), and
# IxI = x_im * x_jm. For the measures with matrix_m, x is a dummy and s_im is
# firm i's value in matrix_m for market m.

# Look up firm-level or firm-in-market values from a long table onto the pair
# table, for the focal firm (side = "i") or the rival (side = "j").
.mmc_lookup <- function(pr, tab, value, new_name, side = c("i", "j"), by_market = TRUE) {
  side <- match.arg(side)
  key <- c("date", if (side == "i") "id" else "id_y", if (by_market) "market")
  on <- stats::setNames(c("date", "id", if (by_market) "market"), key)
  pr[tab, (new_name) := v, on = on, env = list(v = paste0("i.", value))]
  set(pr, which(is.na(pr[[new_name]])), new_name, 0)
  invisible(pr)
}

.mmc_measures <- list(

  # Count: M_ijm = I_im I_jm sum_{n != m} I_in I_jn (focal market excluded).
  count = list(
    needs_m = FALSE, presence_only = FALSE, im = "mean", m = "mean",
    pair = function(pr, inp) {
      pr[, mmc_ij := S_ij]
      pr[, mmc_ijm := (S_ij - IxI) * IxI]
    }
  ),

  # Evans & Kessides (1994), count: I_im I_jm sum_n I_in I_jn (focal market included).
  ekcount = list(
    needs_m = FALSE, presence_only = FALSE, im = "mean", m = "mean",
    pair = function(pr, inp) {
      pr[, mmc_ij := S_ij]
      pr[, mmc_ijm := S_ij * IxI]
    }
  ),

  # Boeker et al. (1997): MMC_ij = sum_n I_in I_jn / sum_n I_in.
  bgsm = list(
    needs_m = FALSE, presence_only = FALSE, im = "mean", m = "mean",
    pair = function(pr, inp) {
      W <- inp$P[, list(W_i = sum(x)), by = c("date", "id")]
      .mmc_lookup(pr, W, "W_i", "W_i", "i", by_market = FALSE)
      pr[, mmc_ij := S_ij / W_i]
      pr[, mmc_ijm := IxI * mmc_ij]
    }
  ),

  # Baum & Korn (1996): as Boeker et al. at the dyad level, but the
  # firm-in-market value only averages over multimarket rivals (B_ij = 1 if
  # the pair meets in more than one market).
  bk = list(
    needs_m = FALSE, presence_only = FALSE, im = "bk", m = "mean",
    pair = function(pr, inp) {
      W <- inp$P[, list(W_i = sum(x)), by = c("date", "id")]
      .mmc_lookup(pr, W, "W_i", "W_i", "i", by_market = FALSE)
      pr[, B := as.integer(M_ij > 1L)]
      pr[, mmc_ij := S_ij / W_i]
      pr[, mmc_ijm := IxI * mmc_ij]
    }
  ),

  # Baum & Korn (1999):
  # MMC_ij = sum_n (C_in + C_jn) I_in I_jn / (sum_n I_in + sum_n I_jn)
  # if M_ij > 1, else 0. C_in (the paper's route centrality) is either
  # matrix_m as given (centrality = "given") or firm i's share of its own
  # total in matrix_m, s_in / sum_n s_in (centrality = "share").
  bk1999 = list(
    needs_m = TRUE, presence_only = TRUE, im = "mean", m = "mean",
    pair = function(pr, inp) {
      C <- if (identical(inp$opts$centrality, "given")) {
        inp$S[, list(date, id, market, C = s)]
      } else {
        inp$S[, list(market, C = s / sum(s)), by = c("date", "id")]
      }
      N <- inp$P[, list(N = .N), by = c("date", "id")]
      .mmc_lookup(pr, C, "C", "C_i", "i")
      .mmc_lookup(pr, C, "C", "C_j", "j")
      .mmc_lookup(pr, N, "N", "N_i", "i", by_market = FALSE)
      .mmc_lookup(pr, N, "N", "N_j", "j", by_market = FALSE)
      pr[, mmc_ij := sum((C_i + C_j) * IxI) / (N_i[1L] + N_j[1L]), by = c("date", "id", "id_y")]
      pr[M_ij <= 1L, mmc_ij := 0]
      pr[, mmc_ijm := IxI * mmc_ij]
    }
  ),

  # Chen (1996): MktComm_ij = sum_n (P_in / P_i) (P_jn / P_n), with P_i the
  # total of firm i over all markets and P_n the total of market n over all
  # firms in the data (at the same date).
  chen = list(
    needs_m = TRUE, presence_only = TRUE, im = "mean", m = "mean",
    pair = function(pr, inp) {
      S <- copy(inp$S)
      S[, P_i := s / sum(s), by = c("date", "id")]      # P_in / P_i
      S[, P_n := s / sum(s), by = c("date", "market")]  # P_jn / P_n
      .mmc_lookup(pr, S, "P_i", "s_i", "i")
      .mmc_lookup(pr, S, "P_n", "s_j", "j")
      pr[, mmc_ij := sum(s_i * s_j * IxI), by = c("date", "id", "id_y")]
      pr[, mmc_ijm := IxI * mmc_ij]
    }
  ),

  # Feinberg (1985), sales at risk:
  # SAR_ij = sum_n I_in I_jn S_in; SAR_ijm = I_im I_jm sum_{n != m} I_in I_jn S_in;
  # SAR_im = sum_j SAR_ijm (a sum, not an average);
  # SAR_m = sum_i SAR_im / sum_i S_im.
  feinberg = list(
    needs_m = TRUE, presence_only = TRUE, im = "sum", m = "sales",
    pair = function(pr, inp) {
      .mmc_lookup(pr, inp$S, "s", "s_i", "i")
      pr[, mmc_ij := sum(s_i * IxI), by = c("date", "id", "id_y")]
      pr[, mmc_ijm := IxI * (mmc_ij - s_i)]
    }
  )
)
