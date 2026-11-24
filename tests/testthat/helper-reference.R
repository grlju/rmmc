# A slow, dense reference implementation written directly from the formulas in
# Gimeno & Jeong (2001), Appendix A, with plain loops over firms and markets.
# It shares no code with the package and is used to check the sparse engine.

# X: firms x markets matrix (presence dummies or weights) for one date.
# S: firms x markets matrix of sales (matrix_m) or NULL.
ref_mmc <- function(measure, X, S = NULL, centrality = "share") {
  presence_only <- measure %in% c("bk1999", "chen", "feinberg")
  I <- (X > 0) * 1
  x <- if (presence_only) I else X
  n <- nrow(x)
  M <- ncol(x)
  safe_div <- function(a, b) {
    r <- a / b
    r[!is.finite(r)] <- 0
    r
  }
  if (!is.null(S)) {
    S_i <- rowSums(S)                 # total of each firm (P_i)
    S_n <- colSums(S)                 # total of each market (P_n)
    C <- if (centrality == "given") S else S / ifelse(S_i > 0, S_i, 1)  # C_in = s_in / sum_n s_in
  }

  ij <- Mij <- matrix(0, n, n)
  ijm <- array(0, c(n, n, M))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) next
    shared <- I[i, ] * I[j, ]
    Mij[i, j] <- sum(shared)
    xx <- x[i, ] * x[j, ]
    Sij <- sum(xx)
    val <- switch(measure,
      count = Sij,
      ekcount = Sij,
      bgsm = Sij / sum(x[i, ]),
      bk = Sij / sum(x[i, ]),
      bk1999 = if (Mij[i, j] > 1) sum((C[i, ] + C[j, ]) * shared) / (sum(I[i, ]) + sum(I[j, ])) else 0,
      chen = sum(shared * safe_div(S[i, ], S_i[i]) * safe_div(S[j, ], S_n)),
      feinberg = sum(shared * S[i, ])
    )
    if (Mij[i, j] == 0) val <- 0
    ij[i, j] <- val
    for (m in seq_len(M)) {
      ijm[i, j, m] <- switch(measure,
        count = xx[m] * (Sij - xx[m]),
        ekcount = xx[m] * Sij,
        feinberg = shared[m] * (val - S[i, m]),
        xx[m] * val  # bgsm, bk, bk1999, chen
      )
    }
  }

  im <- matrix(0, n, M)
  im_rival <- matrix(FALSE, n, M)
  for (i in seq_len(n)) for (m in seq_len(M)) {
    if (I[i, m] == 0) next
    rivals <- which(I[, m] == 1 & seq_len(n) != i)
    if (measure == "bk") {
      B <- rivals[Mij[i, rivals] > 1]
      den <- length(B)
      num <- sum(ijm[i, B, m])
    } else if (measure == "feinberg") {
      den <- if (length(rivals) > 0) 1 else 0
      num <- sum(ijm[i, rivals, m])
    } else {
      den <- length(rivals)
      num <- sum(ijm[i, rivals, m])
    }
    im_rival[i, m] <- den > 0
    im[i, m] <- if (den > 0) num / den else 0
  }

  # Baum & Korn (1996) for entry: markets the firm is NOT in,
  # sum_j B_ij x_jm MMC_ij / sum_j B_ij I_jm over rivals present in m.
  entry <- matrix(NA_real_, n, M)
  if (measure == "bk") {
    for (i in seq_len(n)) for (m in seq_len(M)) {
      if (I[i, m] == 1) next
      B <- which(I[, m] == 1 & Mij[i, ] > 1)
      if (length(B) > 0) entry[i, m] <- sum(x[B, m] * ij[i, B]) / length(B)
    }
  }

  Nm <- colSums(I)
  mk <- numeric(M)
  mk_rival <- logical(M)
  for (m in seq_len(M)) {
    if (Nm[m] == 0) next
    firms <- which(I[, m] == 1)
    den <- if (measure == "feinberg") sum(S[firms, m]) else Nm[m]
    mk[m] <- if (den > 0) sum(im[firms, m]) / den else 0
    mk_rival[m] <- any(im_rival[firms, m])
  }

  list(I = I, ij = ij, Mij = Mij, ijm = ijm, im = im, im_rival = im_rival,
       mk = mk, mk_rival = mk_rival, Nm = Nm, entry = entry)
}

ref_avg <- function(v, rival, avg_over) {
  n <- switch(avg_over, nonzero = sum(v != 0), rivals = sum(rival), present = length(v))
  if (n > 0) sum(v) / n else 0
}

# Expected long output for one measure and level, over all dates.
# dt: wide data with columns date, id, markets; sales: same layout or NULL.
ref_long <- function(measure, level, dt, sales, markets, avg_over, centrality = "share", entry = FALSE) {
  out <- list()
  for (d in unique(dt$date)) {
    rows <- dt[date == d]
    X <- as.matrix(rows[, markets, with = FALSE])
    S <- if (is.null(sales)) NULL else {
      srows <- sales[date == d][match(rows$id, id)]
      as.matrix(srows[, markets, with = FALSE])
    }
    r <- ref_mmc(measure, X, S, centrality)
    ids <- rows$id
    n <- length(ids)
    Ni <- rowSums(r$I)
    recs <- list()
    if (level == "dyad") {
      for (i in seq_len(n)) for (j in seq_len(n)) if (i != j && r$Mij[i, j] > 0) {
        recs[[length(recs) + 1]] <- data.table(date = d, id = ids[i], id_y = ids[j], value = r$ij[i, j])
      }
    } else if (level == "dyad market") {
      for (i in seq_len(n)) for (j in seq_len(n)) if (i != j && r$Mij[i, j] > 0) {
        sh <- which(r$I[i, ] * r$I[j, ] == 1)
        v <- r$ijm[i, j, sh]
        recs[[length(recs) + 1]] <- data.table(date = d, id = ids[i], id_y = ids[j], market = markets[sh],
                                               value = v, sum = sum(v), avg = ref_avg(v, rep(TRUE, length(v)), avg_over))
      }
    } else if (level %in% c("firm market", "market firm")) {
      for (i in seq_len(n)) {
        pm <- which(r$I[i, ] == 1)
        if (length(pm) == 0) next
        v <- if (level == "firm market") r$im[i, pm] else r$mk[pm]
        rv <- if (level == "firm market") r$im_rival[i, pm] else r$mk_rival[pm]
        recs[[length(recs) + 1]] <- data.table(date = d, id = ids[i], market = markets[pm],
                                               value = v, sum = sum(v), avg = ref_avg(v, rv, avg_over))
      }
      if (entry) {
        add <- list()
        for (i in seq_len(n)) {
          em <- which(!is.na(r$entry[i, ]))
          if (length(em) == 0) next
          pm <- which(r$I[i, ] == 1)
          v <- r$im[i, pm]
          add[[length(add) + 1]] <- data.table(date = d, id = ids[i], market = markets[em], value = r$entry[i, em],
                                               sum = sum(v), avg = if (length(pm)) ref_avg(v, r$im_rival[i, pm], avg_over) else 0)
        }
        recs <- c(recs, add)
      }
    } else if (level == "market") {
      pm <- which(r$Nm > 0)
      v <- r$mk[pm]
      recs[[1]] <- data.table(date = d, market = markets[pm], value = v, sum = sum(v),
                              avg = ref_avg(v, r$mk_rival[pm], avg_over))
    }
    out[[length(out) + 1]] <- rbindlist(recs)
  }
  rbindlist(out)
}

# Random wide test data: n firms, M markets, D dates. Some firms have no market.
random_data <- function(seed, n = 7, M = 5, D = 3, p = 0.45, weights = FALSE) {
  set.seed(seed)
  markets <- paste0("mk", seq_len(M))
  dt <- CJ(date = 2000 + seq_len(D), id = sprintf("f%02d", seq_len(n)))
  pres <- matrix(rbinom(nrow(dt) * M, 1, p), nrow(dt), M)
  vals <- if (weights) pres * round(runif(length(pres), 0.05, 1), 2) else pres
  dt[, (markets) := as.data.table(vals)]
  # Sales: positive where present, and occasionally where data says absent.
  extra <- matrix(rbinom(length(pres), 1, 0.1), nrow(dt), M) * (1 - pres)
  sales <- copy(dt[, c("date", "id"), with = FALSE])
  sales[, (markets) := as.data.table((pres + extra) * round(runif(length(pres), 1, 100)))]
  list(data = dt, sales = sales, markets = markets)
}

# Run a package function by measure name.
run_mmc <- function(measure, data, sales, ...) {
  f <- get(paste0("mmc_", measure), envir = asNamespace("rmmc"))
  if (measure %in% c("bk1999", "chen", "feinberg")) f(data, sales, ...) else f(data, ...)
}
