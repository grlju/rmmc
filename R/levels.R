# Aggregation of the pair table to the five levels of analysis, firm and market
# summaries, and the long or wide output. Shared by every measure.

# Sum and average of `value` over the rows of each group in `by`.
# avg_over chooses the denominator of the average:
#   "nonzero" rows with a non-zero value
#   "rivals"  rows where the measure has at least one (counted) rival
#   "present" all rows (every market the firm, pair or date has)
.mmc_summarise <- function(tab, by, value, avg_over, prefix = value) {
  sum_name <- paste0("sum_", prefix)
  avg_name <- paste0("avg_", prefix)
  tab[, c(sum_name, avg_name) := {
    total <- sum(v)
    n <- switch(avg_over,
                nonzero = sum(v != 0),
                rivals = sum(has_rival),
                present = .N)
    list(total, if (n > 0) total / n else 0)
  }, by = by, env = list(v = value)]
  invisible(tab)
}

# Firm-in-market values for every (date, firm, market) of the firms in `rows`.
.mmc_firm_market <- function(pr, rows, P, spec) {
  agg <- switch(spec$im,
    mean = pr[, list(num = sum(mmc_ijm), den = .N, n_rivals = .N), by = c("date", "id", "market")],
    bk   = pr[, list(num = sum(B * mmc_ijm), den = sum(B), n_rivals = .N), by = c("date", "id", "market")],
    sum  = pr[, list(num = sum(mmc_ijm), den = 1L, n_rivals = .N), by = c("date", "id", "market")]
  )
  Nm <- P[, list(N_m = .N), by = c("date", "market")]
  fm <- agg[rows[, c("date", "id", "market"), with = FALSE], on = c("date", "id", "market")]
  fm[Nm, N_m := i.N_m, on = c("date", "market")]
  for (col in c("num", "den", "n_rivals")) set(fm, which(is.na(fm[[col]])), col, 0)
  fm[, has_rival := den > 0]
  fm[, mmc_im := ifelse(has_rival, num / den, 0)]
  fm[]
}

# Market values from the firm-in-market values of all firms.
.mmc_market <- function(fm, inp, spec) {
  mk <- fm[, list(N_m = .N, num = sum(mmc_im), has_rival = any(has_rival)), by = c("date", "market")]
  if (spec$m == "mean") {
    mk[, den := N_m]
  } else {
    sales <- inp$S[inp$P, on = c("date", "id", "market"), nomatch = NULL][
      , list(den = sum(s)), by = c("date", "market")]
    mk[sales, den := i.den, on = c("date", "market")]
    set(mk, which(is.na(mk$den)), "den", 0)
  }
  mk[, mmc_m := ifelse(den > 0, num / den, 0)]
  mk[]
}

# Number of markets each firm is in.
.mmc_ni <- function(P) P[, list(N_i = .N), by = c("date", "id")]

# Every (focal firm, rival) pair at each date, for the wide dyad outputs.
.mmc_pair_grid <- function(inp) {
  focal_firms <- if (is.null(inp$focal)) inp$firms else inp$firms[id %in% inp$focal]
  grid <- inp$firms[focal_firms, on = "date", allow.cartesian = TRUE]
  setnames(grid, c("id", "i.id"), c("id_y", "id"))
  grid[id != id_y, c("date", "id", "id_y"), with = FALSE]
}

# Attach N_i (focal) and N_j (rival) counts of markets.
.mmc_add_n <- function(tab, Ni, rival = TRUE) {
  tab[Ni, N_i := i.N_i, on = c("date", "id")]
  set(tab, which(is.na(tab$N_i)), "N_i", 0L)
  if (rival) {
    tab[Ni, N_j := i.N_i, on = c(date = "date", id_y = "id")]
    set(tab, which(is.na(tab$N_j)), "N_j", 0L)
  }
  invisible(tab)
}

# Wide layout: one column per market (named prefix_<market>), in market_cols
# order, with 0 where a unit has no value for a market.
.mmc_widen <- function(long, rows, by, value, prefix, market_cols, summary_cols) {
  lhs <- paste(by, collapse = " + ")
  w <- if (nrow(long) > 0L) {
    dcast(long, stats::as.formula(paste(lhs, "~ market")), value.var = value, fill = 0)
  } else {
    copy(rows[0L, by, with = FALSE])
  }
  for (mk in setdiff(market_cols, names(w))) set(w, j = mk, value = rep(0, nrow(w)))
  setnames(w, market_cols, paste0(prefix, "_", market_cols))
  summ <- unique(long[, c(by, summary_cols), with = FALSE], by = by)
  out <- merge(w[rows, on = by], summ, by = by, all.x = TRUE, sort = FALSE)
  value_cols <- c(paste0(prefix, "_", market_cols), summary_cols)
  for (col in value_cols) set(out, which(is.na(out[[col]])), col, 0)
  setcolorder(out, c(setdiff(names(rows), value_cols), value_cols))
  setorderv(out, by)
  out
}

# Baum & Korn (1996) for entry: firm-in-market values for markets the focal
# firm is NOT present in, from its multimarket rivals (B_ij = 1) that are:
#   MMC_im = sum_j B_ij x_jm MMC_ij / sum_j B_ij I_jm
# Only markets with at least one such rival get a row. The firm's sum_ and
# avg_ columns stay those of its present markets.
.mmc_add_entry <- function(unit, pr, inp, summary_cols) {
  pairs <- unique(pr[M_ij > 1L, c("date", "id", "id_y", "mmc_ij"), with = FALSE],
                  by = c("date", "id", "id_y"))
  rival_markets <- inp$P[, list(date, id_y = id, market, x_j = x)]
  ex <- merge(pairs, rival_markets, by = c("date", "id_y"), allow.cartesian = TRUE)
  ex <- ex[!inp$P, on = c("date", "id", "market")]
  if (nrow(ex) == 0L) return(unit)
  ex <- ex[, list(num = sum(x_j * mmc_ij), den = .N), by = c("date", "id", "market")]
  Nm <- inp$P[, list(N_m = .N), by = c("date", "market")]
  ex[Nm, N_m := i.N_m, on = c("date", "market")]
  ex[, `:=`(n_rivals = N_m, has_rival = TRUE, mmc_im = num / den, present = FALSE)]
  summ <- unique(unit[, c("date", "id", summary_cols), with = FALSE], by = c("date", "id"))
  ex <- merge(ex, summ, by = c("date", "id"), all.x = TRUE)
  for (col in summary_cols) set(ex, which(is.na(ex[[col]])), col, 0)
  rbind(unit, ex, use.names = TRUE)
}

# Run one measure at one level. Called by every exported mmc_* function.
.mmc_run <- function(measure, data, matrix_m = NULL, firm_col, date_col = NA, level,
                     market_cols = NA, subset_row = FALSE, fill_na = FALSE,
                     full_result = FALSE, focal = NULL,
                     format = c("long", "wide"), avg_over = c("nonzero", "rivals", "present"),
                     opts = list()) {
  spec <- .mmc_measures[[measure]]
  entry <- isTRUE(opts$entry)
  if (entry && !identical(level, "firm market")) {
    stop("entry = TRUE is only available at the \"firm market\" level.")
  }
  if (missing(level) || length(level) != 1L || !(level %in% .mmc_levels)) {
    stop("Measure is not one of dyad, dyad market, firm market, market or market firm.")
  }
  format <- match.arg(format)
  avg_over <- match.arg(avg_over)
  if (isTRUE(full_result) && format == "wide") {
    warning("full_result is only used with format = \"long\".")
    full_result <- FALSE
  }

  inp <- .mmc_prepare(data, matrix_m, firm_col, date_col, market_cols,
                      subset_row, fill_na, focal, spec$needs_m)
  inp$opts <- opts
  if (spec$presence_only) inp$P[, x := 1]
  if (level == "market" && !is.null(inp$focal)) {
    warning("focal is ignored at the market level: market values use all firms.")
  }

  # Market values need the firm-in-market values of every firm, so all firms
  # are focal there; other levels only expand the focal firms.
  all_focal <- level %in% c("market", "market firm")
  pr <- .mmc_pairs(inp$P, if (all_focal) NULL else inp$focal)
  spec$pair(pr, inp)
  Ni <- .mmc_ni(inp$P)
  focal_rows <- if (is.null(inp$focal)) inp$P else inp$P[id %in% inp$focal]
  focal_firms <- if (is.null(inp$focal)) inp$firms else inp$firms[id %in% inp$focal]
  order_market <- function(tab) {
    tab[, market := factor(market, levels = market_cols)]
    setorderv(tab, intersect(c("date", "id", "id_y", "market"), names(tab)))
    tab[, market := as.character(market)]
  }

  if (level == "dyad") {
    res <- unique(pr, by = c("date", "id", "id_y"))
    keep <- c("date", "id", "id_y", "mmc_ij", if (full_result) setdiff(names(pr),
              c("date", "id", "id_y", "market", "x_i", "x_j", "IxI", "mmc_ijm", "s_i", "s_j", "C_i", "C_j", "N_i", "N_j")))
    res <- res[, unique(keep), with = FALSE]
    if (format == "wide") {
      grid <- .mmc_pair_grid(inp)
      res <- res[grid, on = c("date", "id", "id_y")]
      set(res, which(is.na(res$mmc_ij)), "mmc_ij", 0)
    }
    .mmc_add_n(res, Ni)
    setcolorder(res, c("date", "id", "N_i", "id_y", "N_j", "mmc_ij"))
    setorderv(res, c("date", "id", "id_y"))

  } else if (level == "dyad market") {
    pr[, has_rival := TRUE]
    .mmc_summarise(pr, c("date", "id", "id_y"), "mmc_ijm", avg_over)
    .mmc_add_n(pr, Ni)
    summary_cols <- c("sum_mmc_ijm", "avg_mmc_ijm")
    if (format == "wide") {
      res <- .mmc_widen(pr, .mmc_pair_grid(inp), c("date", "id", "id_y"), "mmc_ijm", "mmc_ijm",
                        market_cols, summary_cols)
      .mmc_add_n(res, Ni)
      setcolorder(res, c("date", "id", "N_i", "id_y", "N_j"))
    } else {
      keep <- c("date", "id", "N_i", "id_y", "N_j", "market", "mmc_ijm", summary_cols)
      if (full_result) keep <- c(keep, setdiff(names(pr), c(keep, "has_rival")))
      res <- order_market(pr[, keep, with = FALSE])
    }

  } else {
    fm <- .mmc_firm_market(pr, if (all_focal) inp$P else focal_rows, inp$P, spec)

    if (level == "firm market") {
      unit <- fm
      value <- "mmc_im"
      extra <- c("N_m", "n_rivals", "num", "den")
    } else {
      mk <- .mmc_market(fm, inp, spec)
      if (level == "market") {
        unit <- mk
        value <- "mmc_m"
        extra <- c("num", "den")
      } else {
        unit <- mk[focal_rows[, c("date", "id", "market"), with = FALSE],
                   on = c("date", "market"),
                   list(date, id, market, N_m = x.N_m, mmc_mi = x.mmc_m, has_rival = x.has_rival)]
        value <- "mmc_mi"
        extra <- "N_m"
      }
    }

    by <- if (level == "market") "date" else c("date", "id")
    .mmc_summarise(unit, by, value, avg_over)
    summary_cols <- paste0(c("sum_", "avg_"), value)
    if (entry) {
      unit[, present := TRUE]
      unit <- .mmc_add_entry(unit, pr, inp, summary_cols)
      extra <- c(extra, "present")
    }
    if (level != "market") .mmc_add_n(unit, Ni, rival = FALSE)

    if (format == "wide") {
      rows <- if (level == "market") unique(inp$firms[, "date"]) else focal_firms
      res <- .mmc_widen(unit, rows, by, value, value, market_cols, summary_cols)
      if (level != "market") {
        .mmc_add_n(res, Ni, rival = FALSE)
        setcolorder(res, c("date", "id", "N_i"))
      }
    } else {
      keep <- c(by, if (level != "market") "N_i", "market",
                if (level == "market") "N_m", if (entry) "present", value, summary_cols)
      if (full_result) keep <- c(keep, setdiff(extra, keep))
      res <- order_market(unit[, keep, with = FALSE])
    }
  }

  .mmc_finish(res, firm_col, date_col)
}

# Restore the caller's column names and drop the placeholder date.
.mmc_finish <- function(res, firm_col, date_col) {
  if (is.na(date_col)) {
    res[, date := NULL]
  } else {
    setnames(res, "date", date_col)
  }
  setnames(res, intersect(c("id", "id_y"), names(res)),
           c(firm_col, paste0(firm_col, "_y"))[c("id", "id_y") %in% names(res)])
  setattr(res, "sorted", NULL)
  res[]
}
