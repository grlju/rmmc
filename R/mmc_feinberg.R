#' @title mmc_feinberg
#'
#' @description It calculates multi-market contacts with Feinberg's (1985) implementation.
#'
#' @param data A data.table or data.frame (or an object coercible to a data.table) consisting of rows of firms and columns of markets.
#' @param matrix_m A supplement data to indicate the revenue of firms in markets. Ideally It should have the same dimension and order as data.
#' @param firm_col A string with the name of the column of firm identifiers. It must be provided.
#' @param date_col A string with the name of the column of dates. Optional. If provided, the measures will be computed for each date separately.
#' @param level A string. It must be either "dyad", "dyad market", "firm market", "market" or "market firm". It must be provided.
#' @param market_cols A sting with names of all columns for the markets for which multimarket contact is to be computed.
#' @param subset_row A vector of logical expression indicating elements or rows to keep.
#' @param fill_na Logical. True to fill NAs in the data with 0.
#' @param full_result Logical. True to display all intermediate working numbers.
#' @param write_to_disk Logical. If the output is too large to fit in memory, it is saved to user's current directory as a compress .gz file.
#' @param chunk_size An integer. Size of chunks for processng and saving the data.
#' @param saveto Path for saving the result.
#'
#' @return The function outputs a data.table with the following columns:
#'
#' If the \emph{dyad} level is specified:
#'
#' \itemize{
#' \item date: The date, if specified
#'
#' \item id: Identifier of the focal firm
#'
#' \item N_i: The number of markets the focal firm participates in.
#'
#' \item id.y: Identifier of the other firm in the dyad
#'
#' \item N_j: The number of markets the other firm in the dyad participates in.
#'
#' \item mmc_ij: The dyadic degree of multimarket contact between a focal firm (firm i) and the other firm in the dyad (firm j) across all the markets in which both firms are present.
#' }
#'
#'
#' If full_result = TRUE is specified, in addition to the variable described above, the following is returned:
#'
#' \itemize{
#' \item market_cols: The columns that indicate market participation for the focal firm
#'
#' \item market_cols.y: The columns that indicate market participation for the other firm in the dyad
#'
#' \item market_cols.asset: The assets of the focal firm (firm i) from market m
#' }
#'
#' If the \emph{dyad market} level is specified:
#'
#' \itemize{
#' \item date: The date, if specified
#'
#' \item id: Identifier of the focal firm
#'
#' \item N_i: The number of markets the focal firm participates in.
#'
#' \item id.y: Identifier of the other firm in the dyad
#'
#' \item N_j: The number of markets the other firm in the dyad participates in.
#'
#' \item mmc_ij m: A set of columns that represent the dyadic degree of multimarket contact between a focal firm (firm i) and the other firm in the dyad (firm j) across all the markets where both firms are present, in each market m in which both firms are present.
#'
#' \item Sum mmc_ijm: The row sum of the mmc_ij m columns
#'
#' \item Avg mmc_ijm: The row average of the mmc_ij m columns
#' }
#'
#' If full_result = TRUE is specified, in addition to the variable described above, the following is returned:
#'
#' \itemize{
#' \item market_cols: The columns that indicate market participation for the focal firm
#'
#' \item market_cols.y: The columns that indicate market participation for the other firm in the dyad
#'
#' \item IxI: A set of columns that indicate 1 when the focal firm and the other firm in the dyad are present in market m, else 0.
#'
#' \item IxIxS m: It is SUM_n!=m IxI * matrix_p
#' }
#'
#'
#' If the \emph{firm market} level is specified:
#'
#' \itemize{
#' \item date: The date, if specified
#'
#' \item id: Identifier of the focal firm
#'
#' \item N_i: The number of markets the focal firm participates in.
#'
#' \item mmc_im: A set of columns that represent the firm-in-market multimarket contact for a focal firm (firm i), in each market m in which the focal firm (firm i) is present.
#'
#' \item Sum mmc_im: The row sum of the mmc_im columns
#'
#' \item Avg mmc_im: The row average of the mmc_im columns
#' }
#'
#' If full_result = TRUE is specified, in addition to the variable described above, the following is returned:
#'
#' \itemize{
#' \item mmc_ijm m: A set of columns representing the multimarket contact a focal firm (firm i) experiences in each market m in which it is present.
#' }
#'
#'
#' If the \emph{market} level is specified:
#'
#' \itemize{
#' \item date: The date, if specified
#'
#' \item mmc_m: The overall degree of multimarket contact among the firms present in a focal market m
#'
#' \item Sum mmc_m: The row sum of the mmc_m columns
#'
#' \item Avg mmc_m: The row average of the mmc_m columns
#' }
#'
#' If full_result = TRUE is specified, in addition to the variable described above, the following is returned:
#'
#' \itemize{
#' \item mmc_im: A set of columns representing the multimarket contact in each market m.
#'
#' \item S_im m: The total assets from market m
#' }
#'
#' If the \emph{market firm} level is specified:
#'
#' \itemize{
#' \item date: The date, if specified
#'
#' \item id: Identifier of the focal firm
#'
#' \item mmc_mi: A set of columns representing the overall market level multimarket contact in each market m in which the focal firm (firm i) is present.
#'
#' \item Sum mmc_mi: The row sum of the mmc_mi columns
#'
#' \item Avg mmc_mi: The row average of the mmc_mi columns
#' }
#'
#' If full_result = TRUE is specified, in addition to the variable described above, the following is returned:
#'
#' \itemize{
#' \item market_cols: The columns that indicate market participation for the focal firm
#'
#' \item N_i: The number of markets the focal firm participates in.
#'
#' \item N_m: The number of firms in market m
#'
#' \item mmc_m: The overall degree of multimarket contact among the firms present in a focal market m
#' }
#'
#' @import data.table
#' @export
#'
#' @examples
#' data = rmmc:::data
#' m = rmmc:::m
#' mmc_feinberg(data, m, "id", "year", "dyad", c("m1", "m2", "m3", "m4"))
mmc_feinberg = function(data, matrix_m, firm_col, date_col = NA, level, market_cols = NA, subset_row = FALSE, fill_na = FALSE, full_result = FALSE,
                        write_to_disk = FALSE, chunk_size = NA, saveto = NA){

  # coerce to dt
  data = data.table::setDT(data)

  # if firm_col is NA
  if (is.na(firm_col)){
    stop("Please provide firm_col.")
  }

  # level is not dyad/dyad market/...
  # reject
  if (!(level %in% c("dyad", "dyad market", "firm market", "market", "market firm"))){
    stop("Measure is not one of dyad, dyad market, firm market, market or market firm.")
  }

  # if market_cols is unprovided
  # reject
  if (all(is.na(market_cols))){
    stop("Please provide the markets.")
  }

  # check whether firm identifiers are unique by date
  if (is.na(date_col)){
    is_unique = anyDuplicated(data, by = firm_col)
    if (is_unique != 0) {
      stop("Firm identifiers are not unique.")
    }
  } else if (!is.na(date_col)){
    is_unique = anyDuplicated(data, by = c(date_col, firm_col))
    if (is_unique != 0) {
      stop("Firm identifiers are not unique by date.")
    }
  }

  # check whether arguments for write_to_disk are valid
  if (isTRUE(write_to_disk) & is.na(chunk_size)){
    stop("Please enter chunk size.")
  } else if (isTRUE(write_to_disk) & ((chunk_size <= 0) | chunk_size > nrow(data))){
    stop("Please enter correct chunk size. It must be between 1 to nrow(data).")
  }

  # subset data if applicable
  if (is.na(date_col)){
    data = subset(data, select = c(firm_col, market_cols))
  } else if (!is.na(date_col)){
    data = subset(data, select = c(date_col, firm_col, market_cols))
  }

  if (!isFALSE(subset_row)){
    data = subset(data, subset = subset_row)
  }

  # fill NAs to 0s in only market columns
  if (isTRUE(fill_na)){
    setnafill(data, cols = market_cols, fill = 0)
  }

  # markets' names
  market = market_cols
  market_y = paste0(market_cols, ".y")

  if (!is.na(date_col)){
    setorderv(data, c(date_col, firm_col), c(1, 1))
    setorderv(matrix_m, c(date_col, firm_col), c(1, 1))
  } else {
    setorderv(data, c(firm_col), c(1))
    setorderv(matrix_m, c(firm_col), c(1))
  }

  if (is.na(saveto)){
    save_path = save_path
  } else {
    save_path = saveto
  }

  ################################## Dyad #####################################################
  if (level == "dyad"){
    if (isTRUE(write_to_disk)){
      dir.create(paste0(save_path, "/feinberg"))
      slider_start = seq(1, nrow(data), by = chunk_size)
      slider_end = c(slider_start[2:length(slider_start)] - 1, nrow(data))
      for (i in 1:length(slider_start)){
        matrix_p = copy(matrix_m)
        if (!is.na(date_col)){
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
        } else {
          fake_date = 0
          names(fake_date) = "fake_date"
          data = cbind(fake_date, data)
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
          mmc_ij[, fake_date := NULL]
        }

        mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
        IxI_col = paste("IxI", market)
        mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

        N_i = N_j = NULL
        mmc_ij[, N_i := rowSums(.SD), .SDcols = market]
        mmc_ij[, N_j := rowSums(.SD), .SDcols = market_y]

        if (!is.na(date_col)){
          mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
        } else {
          mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
        }

        IxIxS = paste("IxIxS", market)
        mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
        mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

        if (isFALSE(full_result)){
          mmc_ij[, c(market, market_y, IxI_col, IxIxS, paste0(market, ".asset")) := NULL]
        }
        if (i == 1){
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_ij[, c(date_col, firm_col, "N_i", market, paste0(firm_col, ".y"), "N_j", market_y,
                              IxI_col, paste0(market, ".asset"), "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_ij[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                              IxI_col, paste0(market, ".asset"), "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_ij[, c(date_col, firm_col, "N_i", paste0(firm_col, ".y"), "N_j", "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_ij[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j", "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        } else {
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_ij[, c(date_col, firm_col, "N_i", market, paste0(firm_col, ".y"), "N_j", market_y,
                              IxI_col, paste0(market, ".asset"), "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_ij[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                              IxI_col, paste0(market, ".asset"), "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_ij[, c(date_col, firm_col, "N_i", paste0(firm_col, ".y"), "N_j", "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_ij[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j", "mmc_ij"), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ij.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        }
      }
    } else if (isFALSE(write_to_disk)){
      matrix_p = copy(matrix_m)
      if (!is.na(date_col)){
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
      } else {
        fake_date = 0
        names(fake_date) = "fake_date"
        data = cbind(fake_date, data)
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
        mmc_ij[, fake_date := NULL]
      }

      mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
      IxI_col = paste("IxI", market)
      mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

      N_i = N_j = NULL
      mmc_ij[, N_i := rowSums(.SD), .SDcols = market]
      mmc_ij[, N_j := rowSums(.SD), .SDcols = market_y]

      if (!is.na(date_col)){
        mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
      } else {
        mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
      }

      IxIxS = paste("IxIxS", market)
      mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
      mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

      if (isFALSE(full_result)){
        mmc_ij[, c(market, market_y, IxI_col, IxIxS, paste0(market, ".asset")) := NULL]
      }

      if (isTRUE(full_result) & !is.na(date_col)){
        return(mmc_ij[, c(date_col, firm_col, "N_i", market, paste0(firm_col, ".y"), "N_j", market_y,
                          IxI_col, paste0(market, ".asset"), "mmc_ij"), with = FALSE])
      } else if (isTRUE(full_result) & is.na(date_col)){
        return(mmc_ij[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                          IxI_col, paste0(market, ".asset"), "mmc_ij"), with = FALSE])
      } else if (isFALSE(full_result) & !is.na(date_col)){
        return(mmc_ij[, c(date_col, firm_col, "N_i", paste0(firm_col, ".y"), "N_j", "mmc_ij"), with = FALSE])
      } else if (isFALSE(full_result) & is.na(date_col)){
        return(mmc_ij[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j", "mmc_ij"), with = FALSE])
      }
    }
  }

  if (level == "dyad market"){
    if (isTRUE(write_to_disk)){
      dir.create(paste0(save_path, "/feinberg"))
      slider_start = seq(1, nrow(data), by = chunk_size)
      slider_end = c(slider_start[2:length(slider_start)] - 1, nrow(data))
      for (i in 1:length(slider_start)){
        matrix_p = copy(matrix_m)
        if (!is.na(date_col)){
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
        } else {
          fake_date = 0
          names(fake_date) = "fake_date"
          data = cbind(fake_date, data)
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
          mmc_ij[, fake_date := NULL]
        }

        mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
        IxI_col = paste("IxI", market)
        mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

        if (!is.na(date_col)){
          mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
        } else {
          mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
        }

        IxIxS = paste("IxIxS", market)
        mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
        mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

        N_i = N_j = NULL
        mmc_ij[, N_i := rowSums(.SD), .SDcols = market]
        mmc_ij[, N_j := rowSums(.SD), .SDcols = market_y]

        # calculate dyad-market
        mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]

        if (!is.na(date_col)){
          mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y"), IxI_col, "N_i", "N_j", market, market_y, paste0(market, ".asset")),
                                 with = FALSE], mmc_ijm)
        } else {
          mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y"), IxI_col, "N_i", "N_j",  market, market_y, paste0(market, ".asset")),
                                 with = FALSE], mmc_ijm)
        }

        mmc_ijm_name = paste("mmc_ijm", market)
        mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

        sum_mmc_ijm_name = "Sum mmc_ijm"
        mmc_ijm[, (sum_mmc_ijm_name) := rowSums(.SD, na.rm = TRUE), .SDcols = mmc_ijm_name]
        non_zero_entries_name = "Non zero"
        mmc_ijm[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_ijm_name]
        avg_mmc_ijm_name = "Avg mmc_ijm"
        ans = (mmc_ijm[, sum_mmc_ijm_name, with = FALSE]) / (mmc_ijm[, non_zero_entries_name, with = FALSE])
        ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
        mmc_ijm[, (avg_mmc_ijm_name) := ans]

        if (isFALSE(full_result)){
          mmc_ijm[, c(IxI_col, IxIxS) := NULL]
        }

        if (i == 1){
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_ijm[, c(date_col, firm_col, market, "N_i", paste0(market, ".asset"), paste0(firm_col, ".y"), market_y, "N_j", IxI_col, IxIxS,
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_ijm[, c(firm_col, market, "N_i", paste0(market, ".asset"), paste0(firm_col, ".y"), market_y, "N_j", IxI_col, IxIxS,
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_ijm[, c(date_col, firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_ijm[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        } else {
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_ijm[, c(date_col, firm_col, market, "N_i", paste0(market, ".asset"), paste0(firm_col, ".y"), market_y, "N_j", IxI_col, IxIxS,
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_ijm[, c(firm_col, market, "N_i", paste0(market, ".asset"), paste0(firm_col, ".y"), market_y, "N_j", IxI_col, IxIxS,
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_ijm[, c(date_col, firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_ijm[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                               mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_ijm.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        }
      }
    } else if (isFALSE(write_to_disk)){
      matrix_p = copy(matrix_m)
      if (!is.na(date_col)){
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
      } else {
        fake_date = 0
        names(fake_date) = "fake_date"
        data = cbind(fake_date, data)
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
        mmc_ij[, fake_date := NULL]
      }

      mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
      IxI_col = paste("IxI", market)
      mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

      if (!is.na(date_col)){
        mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
      } else {
        mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
      }

      IxIxS = paste("IxIxS", market)
      mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
      mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

      N_i = N_j = NULL
      mmc_ij[, N_i := rowSums(.SD), .SDcols = market]
      mmc_ij[, N_j := rowSums(.SD), .SDcols = market_y]

      # calculate dyad-market
      mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]

      if (!is.na(date_col)){
        mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y"), IxI_col, "N_i", "N_j", market, market_y, paste0(market, ".asset")),
                               with = FALSE], mmc_ijm)
      } else {
        mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y"), IxI_col, "N_i", "N_j",  market, market_y, paste0(market, ".asset")),
                               with = FALSE], mmc_ijm)
      }

      mmc_ijm_name = paste("mmc_ijm", market)
      mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

      sum_mmc_ijm_name = "Sum mmc_ijm"
      mmc_ijm[, (sum_mmc_ijm_name) := rowSums(.SD, na.rm = TRUE), .SDcols = mmc_ijm_name]
      non_zero_entries_name = "Non zero"
      mmc_ijm[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_ijm_name]
      avg_mmc_ijm_name = "Avg mmc_ijm"
      ans = (mmc_ijm[, sum_mmc_ijm_name, with = FALSE]) / (mmc_ijm[, non_zero_entries_name, with = FALSE])
      ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
      mmc_ijm[, (avg_mmc_ijm_name) := ans]

      if (isFALSE(full_result)){
        mmc_ijm[, c(IxI_col, IxIxS) := NULL]
      }

      if (isTRUE(full_result) & !is.na(date_col)){
        return(mmc_ijm[, c(date_col, firm_col, market, "N_i", paste0(market, ".asset"), paste0(firm_col, ".y"), market_y, "N_j", IxI_col, IxIxS,
                           mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE])
      } else if (isTRUE(full_result) & is.na(date_col)){
        return(mmc_ijm[, c(firm_col, market, "N_i", paste0(market, ".asset"), paste0(firm_col, ".y"), market_y, "N_j", IxI_col, IxIxS,
                           mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE])
      } else if (isFALSE(full_result) & !is.na(date_col)){
        return(mmc_ijm[, c(date_col, firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                           mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE])
      } else if (isFALSE(full_result) & is.na(date_col)){
        return(mmc_ijm[, c(firm_col, "N_i", paste0(firm_col, ".y"), "N_j",
                           mmc_ijm_name, sum_mmc_ijm_name, avg_mmc_ijm_name), with = FALSE])
      }
    }
  }

  if (level == "firm market"){
    if (isTRUE(write_to_disk)){
      dir.create(paste0(save_path, "/feinberg"))
      slider_start = seq(1, nrow(data), by = chunk_size)
      slider_end = c(slider_start[2:length(slider_start)] - 1, nrow(data))
      for (i in 1:length(slider_start)){
        matrix_p = copy(matrix_m)
        if (!is.na(date_col)){
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
        } else {
          fake_date = 0
          names(fake_date) = "fake_date"
          data = cbind(fake_date, data)
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
          mmc_ij[, fake_date := NULL]
        }

        mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
        IxI_col = paste("IxI", market)
        mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

        N_i = NULL
        mmc_ij[, N_i := rowSums(.SD), .SDcols = market]

        if (!is.na(date_col)){
          mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
        } else {
          mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
        }

        IxIxS = paste("IxIxS", market)
        mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
        mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

        # calculate dyad-market
        mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]
        if (!is.na(date_col)){
          mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
        } else {
          mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
        }

        mmc_ijm_name = paste("mmc_ijm", market)
        mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

        # calculate firm-in-market
        if (!is.na(date_col)){
          mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(date_col, firm_col), .SDcols = mmc_ijm_name]
        } else {
          mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(firm_col), .SDcols = mmc_ijm_name]
        }

        mmc_im_name = paste("mmc_im", market)
        setnames(mmc_im, mmc_ijm_name, mmc_im_name)

        sum_mmc_im_name = "Sum mmc_im"
        mmc_im[, (sum_mmc_im_name) := rowSums(.SD), .SDcols = mmc_im_name]
        non_zero_entries_name = "Non zero"
        mmc_im[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_im_name]
        avg_mmc_im_name = "Avg mmc_im"
        ans = (mmc_im[, sum_mmc_im_name, with = FALSE]) / (mmc_im[, non_zero_entries_name, with = FALSE])
        ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
        mmc_im[, (avg_mmc_im_name) := ans]
        mmc_im[, (non_zero_entries_name) := NULL]

        if (!is.na(date_col)){
          mmc_im = merge(mmc_im, unique(mmc_ij[, c(date_col, firm_col, "N_i"), with = FALSE]), by = c(date_col, firm_col))
        } else {
          mmc_im = merge(mmc_im, unique(mmc_ij[, c(firm_col, "N_i"), with = FALSE]), by = c(firm_col))
        }

        if (i == 1){
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_im[, c(date_col, firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_im[, c(firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_im[, c(date_col, firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_im[, c(firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        } else {
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_im[, c(date_col, firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_im[, c(firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_im[, c(date_col, firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_im[, c(firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_im.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        }
      }
    } else if (isFALSE(write_to_disk)){
      matrix_p = copy(matrix_m)
      if (!is.na(date_col)){
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
      } else {
        fake_date = 0
        names(fake_date) = "fake_date"
        data = cbind(fake_date, data)
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
        mmc_ij[, fake_date := NULL]
      }

      mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
      IxI_col = paste("IxI", market)
      mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

      N_i = NULL
      mmc_ij[, N_i := rowSums(.SD), .SDcols = market]

      if (!is.na(date_col)){
        mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
      } else {
        mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
      }

      IxIxS = paste("IxIxS", market)
      mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
      mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

      # calculate dyad-market
      mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]
      if (!is.na(date_col)){
        mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
      } else {
        mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
      }

      mmc_ijm_name = paste("mmc_ijm", market)
      mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

      # calculate firm-in-market
      if (!is.na(date_col)){
        mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(date_col, firm_col), .SDcols = mmc_ijm_name]
      } else {
        mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(firm_col), .SDcols = mmc_ijm_name]
      }

      mmc_im_name = paste("mmc_im", market)
      setnames(mmc_im, mmc_ijm_name, mmc_im_name)

      sum_mmc_im_name = "Sum mmc_im"
      mmc_im[, (sum_mmc_im_name) := rowSums(.SD), .SDcols = mmc_im_name]
      non_zero_entries_name = "Non zero"
      mmc_im[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_im_name]
      avg_mmc_im_name = "Avg mmc_im"
      ans = (mmc_im[, sum_mmc_im_name, with = FALSE]) / (mmc_im[, non_zero_entries_name, with = FALSE])
      ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
      mmc_im[, (avg_mmc_im_name) := ans]
      mmc_im[, (non_zero_entries_name) := NULL]

      if (!is.na(date_col)){
        mmc_im = merge(mmc_im, unique(mmc_ij[, c(date_col, firm_col, "N_i"), with = FALSE]), by = c(date_col, firm_col))
      } else {
        mmc_im = merge(mmc_im, unique(mmc_ij[, c(firm_col, "N_i"), with = FALSE]), by = c(firm_col))
      }

      if (isTRUE(full_result) & !is.na(date_col)){
        return(mmc_im[, c(date_col, firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE])
      } else if (isTRUE(full_result) & is.na(date_col)){
        return(mmc_im[, c(firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE])
      } else if (isFALSE(full_result) & !is.na(date_col)){
        return(mmc_im[, c(date_col, firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE])
      } else if (isFALSE(full_result) & is.na(date_col)){
        return(mmc_im[, c(firm_col, "N_i", mmc_im_name, sum_mmc_im_name, avg_mmc_im_name), with = FALSE])
      }
    }
  }

  if (level == "market"){
    if (isTRUE(write_to_disk)){
      dir.create(paste0(save_path, "/feinberg"))
      slider_start = seq(1, nrow(data), by = chunk_size)
      slider_end = c(slider_start[2:length(slider_start)] - 1, nrow(data))
      for (i in 1:length(slider_start)){
        matrix_p = copy(matrix_m)
        if (!is.na(date_col)){
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
        } else {
          fake_date = 0
          names(fake_date) = "fake_date"
          data = cbind(fake_date, data)
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
          mmc_ij[, fake_date := NULL]
        }

        mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
        IxI_col = paste("IxI", market)
        mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

        if (!is.na(date_col)){
          mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
        } else {
          mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
        }

        IxIxS = paste("IxIxS", market)
        mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
        mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

        # calculate dyad-market
        mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]

        if (!is.na(date_col)){
          mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
        } else {
          mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
        }

        mmc_ijm_name = paste("mmc_ijm", market)
        mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

        # calculate firm-in-market
        if (!is.na(date_col)){
          mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(date_col, firm_col), .SDcols = mmc_ijm_name]
        } else {
          mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(firm_col), .SDcols = mmc_ijm_name]
        }

        mmc_im_name = paste("mmc_im", market)
        setnames(mmc_im, mmc_ijm_name, mmc_im_name)

        # calculate market
        if (!is.na(date_col)){
          mmc_m = mmc_im[, lapply(.SD, sum), by = c(date_col), .SDcols = mmc_im_name]
        } else {
          mmc_m = mmc_im[, lapply(.SD, sum), .SDcols = mmc_im_name]
        }

        if (!is.na(date_col)){
          s_im = matrix_p[, lapply(.SD, sum), by = c(date_col), .SDcols = market]
        } else {
          s_im = matrix_p[, lapply(.SD, sum), .SDcols = market]
        }

        s_im_name = paste("S_im", market)
        setnames(s_im, market, s_im_name)

        if (!is.na(date_col)){
          mmc_m = merge(mmc_m, s_im, by = c(date_col))
        } else {
          mmc_m = cbind(mmc_m, s_im)
        }

        mmc_m_name = paste("mmc", market)
        mmc_m[, (mmc_m_name) := mmc_m[, (mmc_im_name), with = FALSE] /
                (mmc_m[, (s_im_name), with = FALSE])]

        if (isFALSE(full_result)){
          mmc_m[, c(mmc_im_name, s_im_name) := NULL]
        }

        if (i == 1){
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        } else {
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        }
      }

      mmc_m = fread(paste0(save_path, "/feinberg", "/", "mmc_m.gz"))

      if (!is.na(date_col)){
        mmc_m = mmc_m[, lapply(.SD, sum), by = c(date_col), .SDcols = mmc_m_name]
      } else {
        mmc_m = mmc_m[, lapply(.SD, sum), .SDcols = mmc_m_name]
      }

      sum_mmc_m_name = "Sum mmc_m"
      mmc_m[, (sum_mmc_m_name) := rowSums(.SD), .SDcols = mmc_m_name]
      non_zero_entries_name = "Non zero"
      mmc_m[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_m_name]
      avg_mmc_m_name = "Avg mmc_m"
      ans = (mmc_m[, sum_mmc_m_name, with = FALSE]) / (mmc_m[, non_zero_entries_name, with = FALSE])
      ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
      mmc_m[, (avg_mmc_m_name) := ans]
      mmc_m[, (non_zero_entries_name) := NULL]

      fwrite(mmc_m, paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
             append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)

    } else if (isFALSE(write_to_disk)){
      matrix_p = copy(matrix_m)
      if (!is.na(date_col)){
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
      } else {
        fake_date = 0
        names(fake_date) = "fake_date"
        data = cbind(fake_date, data)
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
        mmc_ij[, fake_date := NULL]
      }

      mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
      IxI_col = paste("IxI", market)
      mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

      if (!is.na(date_col)){
        mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
      } else {
        mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
      }

      IxIxS = paste("IxIxS", market)
      mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
      mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

      # calculate dyad-market
      mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]

      if (!is.na(date_col)){
        mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
      } else {
        mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
      }

      mmc_ijm_name = paste("mmc_ijm", market)
      mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

      # calculate firm-in-market
      if (!is.na(date_col)){
        mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(date_col, firm_col), .SDcols = mmc_ijm_name]
      } else {
        mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(firm_col), .SDcols = mmc_ijm_name]
      }

      mmc_im_name = paste("mmc_im", market)
      setnames(mmc_im, mmc_ijm_name, mmc_im_name)

      # calculate market
      if (!is.na(date_col)){
        mmc_m = mmc_im[, lapply(.SD, sum), by = c(date_col), .SDcols = mmc_im_name]
      } else {
        mmc_m = mmc_im[, lapply(.SD, sum), .SDcols = mmc_im_name]
      }

      if (!is.na(date_col)){
        s_im = matrix_p[, lapply(.SD, sum), by = c(date_col), .SDcols = market]
      } else {
        s_im = matrix_p[, lapply(.SD, sum), .SDcols = market]
      }

      s_im_name = paste("S_im", market)
      setnames(s_im, market, s_im_name)

      if (!is.na(date_col)){
        mmc_m = merge(mmc_m, s_im, by = c(date_col))
      } else {
        mmc_m = cbind(mmc_m, s_im)
      }

      mmc_m_name = paste("mmc", market)
      mmc_m[, (mmc_m_name) := mmc_m[, (mmc_im_name), with = FALSE] /
              (mmc_m[, (s_im_name), with = FALSE])]

      if (isFALSE(full_result)){
        mmc_m[, c(mmc_im_name, s_im_name) := NULL]
      }

      sum_mmc_m_name = "Sum mmc_m"
      mmc_m[, (sum_mmc_m_name) := rowSums(.SD), .SDcols = mmc_m_name]
      non_zero_entries_name = "Non zero"
      mmc_m[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_m_name]
      avg_mmc_m_name = "Avg mmc_m"
      ans = (mmc_m[, sum_mmc_m_name, with = FALSE]) / (mmc_m[, non_zero_entries_name, with = FALSE])
      ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
      mmc_m[, (avg_mmc_m_name) := ans]
      mmc_m[, (non_zero_entries_name) := NULL]

      if (isTRUE(full_result) & !is.na(date_col)){
        return(mmc_m[, c(date_col, mmc_im_name, s_im_name, mmc_m_name, sum_mmc_m_name, avg_mmc_m_name), with = FALSE])
      } else if (isTRUE(full_result) & is.na(date_col)){
        return(mmc_m[, c(mmc_im_name, s_im_name, mmc_m_name, sum_mmc_m_name, avg_mmc_m_name), with = FALSE])
      } else if (isFALSE(full_result) & !is.na(date_col)){
        return(mmc_m[, c(date_col, mmc_m_name, sum_mmc_m_name, avg_mmc_m_name), with = FALSE])
      } else if (isFALSE(full_result) & is.na(date_col)){
        return(mmc_m[, c(mmc_m_name, sum_mmc_m_name, avg_mmc_m_name), with = FALSE])
      }
    }
  }

  if (level == "market firm"){
    if (isTRUE(write_to_disk)){
      dir.create(paste0(save_path, "/feinberg"))
      slider_start = seq(1, nrow(data), by = chunk_size)
      slider_end = c(slider_start[2:length(slider_start)] - 1, nrow(data))
      for (i in 1:length(slider_start)){
        matrix_p = copy(matrix_m)
        if (!is.na(date_col)){
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
        } else {
          fake_date = 0
          names(fake_date) = "fake_date"
          data = cbind(fake_date, data)
          mmc_ij = merge(data[slider_start[i]:slider_end[i], ], data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
          mmc_ij[, fake_date := NULL]
        }

        mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
        IxI_col = paste("IxI", market)
        mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

        if (!is.na(date_col)){
          mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
        } else {
          mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
        }

        IxIxS = paste("IxIxS", market)
        mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
        mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

        # calculate dyad-market
        mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]

        if (!is.na(date_col)){
          mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
        } else {
          mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
        }

        mmc_ijm_name = paste("mmc_ijm", market)
        mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

        # calculate firm-in-market
        if (!is.na(date_col)){
          mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(date_col, firm_col), .SDcols = mmc_ijm_name]
        } else {
          mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(firm_col), .SDcols = mmc_ijm_name]
        }

        mmc_im_name = paste("mmc_im", market)
        setnames(mmc_im, mmc_ijm_name, mmc_im_name)

        # calculate market
        if (!is.na(date_col)){
          mmc_m = mmc_im[, lapply(.SD, sum), by = c(date_col), .SDcols = mmc_im_name]
        } else {
          mmc_m = mmc_im[, lapply(.SD, sum), .SDcols = mmc_im_name]
        }

        if (!is.na(date_col)){
          s_im = matrix_p[, lapply(.SD, sum), by = c(date_col), .SDcols = market]
        } else {
          s_im = matrix_p[, lapply(.SD, sum), .SDcols = market]
        }

        s_im_name = paste("S_im", market)
        setnames(s_im, market, s_im_name)

        if (!is.na(date_col)){
          mmc_m = merge(mmc_m, s_im, by = c(date_col))
        } else {
          mmc_m = cbind(mmc_m, s_im)
        }

        mmc_m_name = paste("mmc", market)
        mmc_m[, (mmc_m_name) := mmc_m[, (mmc_im_name), with = FALSE] /
                (mmc_m[, (s_im_name), with = FALSE])]

        if (isFALSE(full_result)){
          mmc_m[, c(mmc_im_name, s_im_name) := NULL]
        }

        if (i == 1){
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE
            )
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        } else {
          if (isTRUE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isTRUE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_im_name, s_im_name, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & !is.na(date_col)){
            fwrite(mmc_m[, c(date_col, mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          } else if (isFALSE(full_result) & is.na(date_col)){
            fwrite(mmc_m[, c(mmc_m_name), with = FALSE],
                   paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
                   append = TRUE, col.names = FALSE, row.names = FALSE, compress = "auto", showProgress = TRUE)
          }
        }
      }

      mmc_m = fread(paste0(save_path, "/feinberg", "/", "mmc_m.gz"))

      if (!is.na(date_col)){
        mmc_m = mmc_m[, lapply(.SD, sum), by = c(date_col), .SDcols = mmc_m_name]
      } else {
        mmc_m = mmc_m[, lapply(.SD, sum), .SDcols = mmc_m_name]
      }

      fwrite(mmc_m, paste0(save_path, "/feinberg", "/", "mmc_m.gz"),
             append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)

      mmc_mi_name = paste("mmc_mi", market)
      mmc_mi = cbind(data, mmc_m[, (mmc_m_name), with = FALSE])
      mmc_mi[, (mmc_mi_name) := mmc_mi[, (market), with = FALSE] * mmc_mi[, (mmc_m_name), with = FALSE]]

      sum_mmc_mi_name = "Sum mmc_mi"
      mmc_mi[, (sum_mmc_mi_name) := rowSums(.SD), .SDcols = mmc_mi_name]
      non_zero_entries_name = "Non zero"
      mmc_mi[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_mi_name]
      avg_mmc_mi_name = "Avg mmc_mi"
      ans = (mmc_mi[, sum_mmc_mi_name, with = FALSE]) / (mmc_mi[, non_zero_entries_name, with = FALSE])
      ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
      mmc_mi[, (avg_mmc_mi_name) := ans]
      mmc_mi[, (non_zero_entries_name) := NULL]

      N_i = NULL
      mmc_mi[, N_i := rowSums(.SD), .SDcols = market]

      if (isTRUE(full_result) & !is.na(date_col)){
        fwrite(mmc_mi[, c(date_col, firm_col, market, "N_i", n_mkt_name, mmc_m_name, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE],
               paste0(save_path, "/feinberg", "/", "mmc_mi.gz"),
               append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
      } else if (isTRUE(full_result) & is.na(date_col)){
        fwrite(mmc_mi[, c(firm_col, market, "N_i", mmc_m_name, n_mkt_name, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE],
               paste0(save_path, "/feinberg", "/", "mmc_mi.gz"),
               append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
      } else if (isFALSE(full_result) & !is.na(date_col)){
        fwrite(mmc_mi[, c(date_col, firm_col, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE],
               paste0(save_path, "/feinberg", "/", "mmc_mi.gz"),
               append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
      } else if (isFALSE(full_result) & is.na(date_col)){
        fwrite(mmc_mi[, c(firm_col, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE],
               paste0(save_path, "/feinberg", "/", "mmc_mi.gz"),
               append = FALSE, col.names = TRUE, row.names = FALSE, compress = "auto", showProgress = TRUE)
      }
    } else if (isFALSE(write_to_disk)){
      matrix_p = copy(matrix_m)
      if (!is.na(date_col)){
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = date_col, suffixes = c("", ".y"))
      } else {
        fake_date = 0
        names(fake_date) = "fake_date"
        data = cbind(fake_date, data)
        mmc_ij = merge(data, data, all = TRUE, allow.cartesian = TRUE, by = c("fake_date"), suffixes = c("", ".y"))
        mmc_ij[, fake_date := NULL]
        data[, fake_date := NULL]
      }

      mmc_ij = mmc_ij[mmc_ij[[firm_col]] != mmc_ij[[paste0(firm_col, ".y")]]]
      IxI_col = paste("IxI", market)
      mmc_ij[, (IxI_col) := mmc_ij[ , market, with = FALSE] * mmc_ij[, market_y, with = FALSE]]

      N_i = NULL
      mmc_ij[, N_i := rowSums(.SD), .SDcols = market]

      if (!is.na(date_col)){
        mmc_ij = merge(mmc_ij, matrix_p, by = c(date_col, firm_col), suffixes = c("", ".asset"))
      } else {
        mmc_ij = merge(mmc_ij, matrix_p, all = TRUE, by = c(firm_col), suffixes = c("", ".asset"))
      }

      IxIxS = paste("IxIxS", market)
      mmc_ij[, (IxIxS) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ij[, paste0(market, ".asset"), with = FALSE]]
      mmc_ij[, mmc_ij := rowSums(.SD), .SDcols = IxIxS]

      # calculate dyad-market
      mmc_ijm = mmc_ij[, lapply(.SD, function(x) mmc_ij - x), .SDcols = IxIxS]

      if (!is.na(date_col)){
        mmc_ijm = cbind(mmc_ij[, c(date_col, firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
      } else {
        mmc_ijm = cbind(mmc_ij[, c(firm_col, paste0(firm_col, ".y")), with = FALSE], mmc_ijm)
      }

      mmc_ijm_name = paste("mmc_ijm", market)
      mmc_ijm[, (mmc_ijm_name) := mmc_ij[, (IxI_col), with = FALSE] * mmc_ijm[, (IxIxS), with = FALSE]]

      # calculate firm-in-market
      if (!is.na(date_col)){
        mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(date_col, firm_col), .SDcols = mmc_ijm_name]
      } else {
        mmc_im = mmc_ijm[, lapply(.SD, sum), by = c(firm_col), .SDcols = mmc_ijm_name]
      }

      mmc_im_name = paste("mmc_im", market)
      setnames(mmc_im, mmc_ijm_name, mmc_im_name)

      # calculate market
      if (!is.na(date_col)){
        mmc_m = mmc_im[, lapply(.SD, sum), by = c(date_col), .SDcols = mmc_im_name]
      } else {
        mmc_m = mmc_im[, lapply(.SD, sum), .SDcols = mmc_im_name]
      }

      if (!is.na(date_col)){
        s_im = matrix_p[, lapply(.SD, sum), by = c(date_col), .SDcols = market]
      } else {
        s_im = matrix_p[, lapply(.SD, sum), .SDcols = market]
      }

      s_im_name = paste("S_im", market)
      setnames(s_im, market, s_im_name)

      if (!is.na(date_col)){
        mmc_m = merge(mmc_m, s_im, by = c(date_col))
      } else {
        mmc_m = cbind(mmc_m, s_im)
      }

      mmc_m_name = paste("mmc", market)
      mmc_m[, (mmc_m_name) := mmc_m[, (mmc_im_name), with = FALSE] /
              (mmc_m[, (s_im_name), with = FALSE])]

      mmc_m[, c(mmc_im_name, s_im_name) := NULL]

      # calculate overall mmc at firm level
      mmc_mi_name = paste("mmc_mi", market)
      mmc_mi = cbind(data, mmc_m[, (mmc_m_name), with = FALSE])
      mmc_mi[, (mmc_mi_name) := mmc_mi[, (market), with = FALSE] * mmc_mi[, (mmc_m_name), with = FALSE]]

      if (isFALSE(full_result)){
        mmc_mi[, c(market, mmc_m_name) := NULL]
      }

      sum_mmc_mi_name = "Sum mmc_mi"
      mmc_mi[, (sum_mmc_mi_name) := rowSums(.SD), .SDcols = mmc_mi_name]
      non_zero_entries_name = "Non zero"
      mmc_mi[, (non_zero_entries_name) := rowSums(.SD != 0), .SDcols = mmc_mi_name]
      avg_mmc_mi_name = "Avg mmc_mi"
      ans = (mmc_mi[, sum_mmc_mi_name, with = FALSE]) / (mmc_mi[, non_zero_entries_name, with = FALSE])
      ans = ifelse(is.nan(ans[[1]]), 0, ans[[1]])
      mmc_mi[, (avg_mmc_mi_name) := ans]
      mmc_mi[, (non_zero_entries_name) := NULL]

      if (!is.na(date_col)){
        mmc_mi = merge(mmc_mi, unique(mmc_ij[, c(date_col, firm_col, "N_i"), with = FALSE]), by = c(date_col, firm_col))
      } else {
        mmc_mi = merge(mmc_mi, unique(mmc_ij[, c(firm_col, "N_i"), with = FALSE]), by = c(firm_col))
      }

      mmc_im_n_mkt = paste("mmc_im_n_mkt", market)

      if (!is.na(date_col)){
        n_mkt = data[, lapply(.SD, sum), by = c(date_col), .SDcols = market]
      } else {
        n_mkt = data[, lapply(.SD, sum), .SDcols = market]
      }

      n_mkt_name = paste0("N_", market)
      setnames(n_mkt, market, n_mkt_name)

      if (!is.na(date_col)){
        mmc_mi = merge(mmc_mi, n_mkt, by = c(date_col))
      } else {
        mmc_mi = cbind(mmc_mi, n_mkt)
      }

      if (isTRUE(full_result) & !is.na(date_col)){
        return(mmc_mi[, c(date_col, firm_col, market, "N_i", n_mkt_name, mmc_m_name, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE])
      } else if (isTRUE(full_result) & is.na(date_col)){
        return(mmc_mi[, c(firm_col, market, "N_i", n_mkt_name, mmc_m_name, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE])
      } else if (isFALSE(full_result) & !is.na(date_col)){
        return(mmc_mi[, c(date_col, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE])
      } else if (isFALSE(full_result) & is.na(date_col)){
        return(mmc_mi[, c(firm_col, mmc_mi_name, sum_mmc_mi_name, avg_mmc_mi_name), with = FALSE])
      }
    }
  }
}
