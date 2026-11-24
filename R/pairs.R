# The sparse pair table.
#
# Every measure is built from the contacts between a focal firm i and a rival j
# in the markets m they share. Instead of crossing every firm with every other
# firm and every market (n^2 * M cells per date), the presence table is joined
# to itself on (date, market). The result has one row per (date, i, j, m) with
# both firms present in m, so its size is the number of actual contacts.
#
# Columns of the returned table:
#   date, id (i), id_y (j), market (m)
#   x_i, x_j  presence or weight of i and j in m
#   IxI       x_i * x_j (1 for presence dummies)
#   M_ij      number of markets i and j share (a count, also for weights)
#   S_ij      sum of IxI over the shared markets (equal to M_ij for dummies)
#
# focal: firm ids to use as i, or NULL for all firms. The rivals j are always
# all firms in P, so that the number of rivals in a market is complete.
.mmc_pairs <- function(P, focal = NULL) {
  Fp <- if (is.null(focal)) P else P[id %in% focal]
  pr <- P[Fp, on = c("date", "market"), allow.cartesian = TRUE, nomatch = NULL]
  setnames(pr, c("id", "x", "i.id", "i.x"), c("id_y", "x_j", "id", "x_i"))
  pr <- pr[id != id_y]
  pr[, IxI := x_i * x_j]
  pr[, `:=`(M_ij = .N, S_ij = sum(IxI)), by = c("date", "id", "id_y")]
  setcolorder(pr, c("date", "id", "id_y", "market", "x_i", "x_j", "IxI", "M_ij", "S_ij"))
  pr[]
}
