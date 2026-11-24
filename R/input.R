# Argument checks and conversion of the wide input (one row per firm and date,
# one column per market) into long tables used by every measure.
#
# Internal column names: date, id, market, x (presence or weight), s (sales).
# When no date column is given, a constant date is added and removed again
# before the result is returned.

.mmc_levels <- c("dyad", "dyad market", "firm market", "market", "market firm")

# Output column names that a firm or date column must not clash with.
.mmc_reserved <- c("market", "N_i", "N_j", "N_m", "M_ij", "S_ij", "x_i", "x_j",
                   "IxI", "num", "den", "n_rivals", "has_rival")

# Turn one wide table into a long table with columns date, id, market, value.
.mmc_long <- function(dt, firm_col, date_col, market_cols, fill_na, what, value_name) {
  has_date <- !is.na(date_col)
  needed <- c(if (has_date) date_col, firm_col, market_cols)
  missing_cols <- setdiff(needed, names(dt))
  if (length(missing_cols) > 0L) {
    stop(what, " is missing these columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  dt <- dt[, needed, with = FALSE]
  if (has_date) {
    setnames(dt, c(date_col, firm_col), c("date", "id"))
  } else {
    setnames(dt, firm_col, "id")
    dt[, date := 0L]
  }

  if (anyDuplicated(dt, by = c("date", "id")) != 0L) {
    stop(if (has_date) "Firm identifiers are not unique by date" else "Firm identifiers are not unique",
         " in ", tolower(what), ".")
  }

  non_numeric <- market_cols[!vapply(dt[, market_cols, with = FALSE], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop("Market columns must be numeric in ", tolower(what), ": ", paste(non_numeric, collapse = ", "), ".")
  }
  if (isTRUE(fill_na)) {
    for (col in market_cols) set(dt, which(is.na(dt[[col]])), col, 0)
  } else if (anyNA(dt[, market_cols, with = FALSE])) {
    stop("The market columns of ", tolower(what), " contain NA. Use fill_na = TRUE to treat them as 0.")
  }
  if (any(vapply(dt[, market_cols, with = FALSE], function(v) any(v < 0), logical(1)))) {
    stop("The market columns of ", tolower(what), " contain negative values.")
  }

  firms <- unique(dt[, list(date, id)])
  long <- melt(dt, id.vars = c("date", "id"), measure.vars = market_cols,
               variable.name = "market", value.name = value_name, variable.factor = FALSE)
  long <- long[long[[value_name]] > 0]
  list(long = long, firms = firms)
}

# Check all arguments and build the inputs for the computation.
# Returns a list with
#   P      presence: date, id, market, x (x > 0 rows only)
#   S      sales from matrix_m: date, id, market, s (s > 0 rows only), or NULL
#   firms  every (date, id) in the data, including firms without any market
#   focal  vector of focal firm ids, or NULL for all firms
.mmc_prepare <- function(data, matrix_m, firm_col, date_col, market_cols,
                         subset_row, fill_na, focal, needs_m) {
  if (missing(firm_col) || length(firm_col) != 1L || is.na(firm_col)) {
    stop("Please provide firm_col.")
  }
  if (length(date_col) != 1L) stop("date_col must be a single column name or NA.")
  if (length(market_cols) == 0L || all(is.na(market_cols))) {
    stop("Please provide the markets.")
  }
  clash <- intersect(c(firm_col, if (!is.na(date_col)) date_col), .mmc_reserved)
  if (length(clash) > 0L) {
    stop("Please rename these columns, which clash with output column names: ",
         paste(clash, collapse = ", "), ".")
  }

  # as.data.table() copies, so the caller's data is never modified by reference.
  dt <- as.data.table(data)
  if (!isFALSE(subset_row)) {
    if (!is.logical(subset_row) || length(subset_row) != nrow(dt)) {
      stop("subset_row must be FALSE or a logical vector with one element per row of data.")
    }
    dt <- dt[!is.na(subset_row) & subset_row]
  }

  pres <- .mmc_long(dt, firm_col, date_col, market_cols, fill_na, "Data", "x")
  out <- list(P = pres$long, S = NULL, firms = pres$firms, focal = NULL)

  if (needs_m) {
    if (is.null(matrix_m)) stop("Please provide matrix_m.")
    sales <- .mmc_long(as.data.table(matrix_m), firm_col, date_col, market_cols,
                       fill_na, "matrix_m", "s")
    # Every firm in the data needs a row in matrix_m. Firms in matrix_m that are
    # not in the (subset) data are dropped, so totals refer to the same firms.
    absent <- sales$firms[pres$firms, on = c("date", "id"), nomatch = NULL, which = TRUE]
    if (length(absent) != nrow(pres$firms)) {
      stop("matrix_m has no row for some firms (and dates) in data.")
    }
    out$S <- sales$long[pres$firms, on = c("date", "id"), nomatch = NULL]
  }

  if (!is.null(focal)) {
    unknown <- setdiff(focal, pres$firms$id)
    if (length(unknown) > 0L) {
      warning(length(unknown), " focal firm(s) are not in the data and are ignored.")
    }
    out$focal <- intersect(focal, pres$firms$id)
    if (length(out$focal) == 0L) stop("None of the focal firms are in the data.")
  }
  out
}
