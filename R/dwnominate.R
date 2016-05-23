# an interface to Keith Poole's DW-NOMINATE FORTRAN77 program.

# limitations:

# 600 legislators per session

# 3600 votes per session

format_column = function(df, name, format, alternative) {
  if (name %in% names(df)) {
    x = df[, name]
  } else {
    x = alternative
  }
  sprintf(format, x)
}

write_rc_data_file = function(rc_list) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    rcl = rc$legis.data
    votes = rc$votes
    codes = rc$codes
    sessions =
      format_column(rcl, "sessionID", "%4d", session)
    leg_ids =
      format_column(rcl, "icpsrLegis", "%6d", rcl$ID)
    state_num =
      format_column(rcl, "icpsrState", "%3d", 0)
    district =
      format_column(rcl, "cd", "%2d", 0)
    state_name =
      format_column(rcl, "state", "%-7s", "NA")
    parties =
      format_column(rcl, "party", "%4d", 0)
    leg_names = sprintf("%-11.11s",
        gsub("[^A-Za-z ()'-]", "", rownames(votes)))
    
    votes[votes %in% codes$yea] = 1
    votes[votes %in% codes$nay] = 6
    votes[votes %in% c(codes$notInLegis, codes$missing)] = 9
    
    # a vector of very long lists of numbers, one for each legislator
    vote_nums =
      apply(votes, 1, FUN = function(x) paste(x, collapse = ""))

    # the lines to be written to the file
    "I4,I6,I3,I2,1X,7A1,1X,I4,1X,11A1,3600I1"
    lines = paste0(sessions, leg_ids, state_num, district, " ",
        state_name, " ", parties, " ", leg_names, vote_nums)
    all_lines = c(all_lines,  lines)
  }
  
  writeLines(all_lines, "rollcall_matrix.vt3")
}

write_transposed_rc_data_file = function(rc_list) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    votes = rc_list[[session]]$votes
    codes = rc_list[[session]]$codes
    sessions = sprintf("%4d", session)
    vote_ids = sprintf("%5d", 1:ncol(votes))
    
    votes[votes %in% codes$yea] = 1
    votes[votes %in% codes$nay] = 6
    votes[votes %in% c(codes$notInLegis, codes$missing)] = 9

    # a vector of very long lists of numbers, one for each legislator
    vote_nums =
      apply(votes, 2, FUN = function(x) paste(x, collapse = ""))

    # the lines to be written to the file
    "I4,I5,1X,600I1"
    lines = paste0(sessions, vote_ids, " ", vote_nums)
    all_lines = c(all_lines, lines)
  }
  writeLines(all_lines, "transposed_rollcall_matrix.vt3")
}

write_leg_file = function(rc_list, wnom_list) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    rcl = rc$legis.data
    wnom = wnom_list[[session]]
    "I4,I6,I3,I2,1X,7A1,1X,I4,1X,11A1,2F7.3,I5"
    sessions =
      format_column(rcl, "sessionID", "%4d", session)
    leg_ids =
      format_column(rcl, "icpsrLegis", "%6d", rcl$ID)
    state_num =
      format_column(rcl, "icpsrState", "%3d", 0)
    district =
      format_column(rcl, "cd", "%2d", 0)
    state_name =
      format_column(rcl, "state", "%-7s", "NA")
    parties =
      format_column(rcl, "party", "%4d", 0)
    leg_names = sprintf("%-11.11s",
        gsub("[^A-Za-z ()'-]", "", rownames(rc$votes)))
    # DW-NOMINATE does not like NA's
    wnom$legislators[is.na(wnom$legislators)] = 0
    coord1 = sprintf("%7.3f", wnom$legislators$coord1D)
    coord2 = sprintf("%7.3f", wnom$legislators$coord2D)
    all_votes = c("correctYea", "wrongYea", "wrongNay",
        "correctNay")
    ## votes = sprintf("%4d", rowSums(wnom$legislators[, all_votes]))
    ## errors = sprintf("%4d",
    ##     rowSums(wnom$legislators[, c("wrongYea", "wrongNay")]))
    ## gmp = sprintf("%6.3f", wnom$legislators$GMP)

    # the lines to be written to the file
    "I4,I6,I3,I2,1X,7A1,1X,I4,1X,11A1,2F7.3,I5"
    lines = paste0(sessions, leg_ids, state_num, district, " ",
        state_name, " ", parties, " ", leg_names, coord1, coord2)#,
        ## " 0.000  0.000  0.000  0.000     0.00000     0.00000",
        ## votes, votes, errors, errors, gmp, gmp)
    all_lines = c(all_lines, lines)
  }
  writeLines(all_lines, "legislator_input.dat")
}

write_bill_file = function(rc_list, wnom_list) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    wnom = wnom_list[[session]]
    "I3,I5,4F7.3"
    sessions = sprintf("%3d", session)
    bill_ids = sprintf("%5d", 1:ncol(rc$votes))
    # DW-NOMINATE does not like NA's
    wnom$rollcalls[is.na(wnom$rollcalls)] = 0
    spread1 = sprintf("%7.3f", wnom$rollcalls$spread1D)
    spread2 = sprintf("%7.3f", wnom$rollcalls$spread2D)
    mid1 = sprintf("%7.3f", wnom$rollcalls$midpoint1D)
    mid2 = sprintf("%7.3f", wnom$rollcalls$midpoint2D)

    # the lines to be written to the file
    lines = paste0(sessions, bill_ids, spread1, mid1, spread2, mid2)
    all_lines = c(all_lines, lines)
  }
  writeLines(all_lines, "rollcall_input.dat")
}

write_session_file = function(rc_list, wnom_list) {
  lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    wnom = wnom_list[[session]]
    session_num = sprintf("%3d", session)
    rollcalls = sprintf("%4d", nrow(wnom$rollcalls))
    legislators = sprintf("%3d", nrow(wnom$legislators))

    # the lines to be written to the file
    lines[session] = paste(session_num, rollcalls, legislators)
  }
  writeLines(lines, "session_info.num")
}

write_start_file = function(rc_list, wnom_list) {
  filenames = c("rollcall_input.dat", "rollcall_output.dat",
      "legislator_input.dat", "legislator_output.dat",
      "session_info.num", "rollcall_matrix.vt3",
      "transposed_rollcall_matrix.vt3")
  ## lines = file.path(getwd(), filenames)
  lines = filenames
  lines[8] = "NOMINAL DYNAMIC-WEIGHTED MULTIDIMENSIONAL UNFOLDING "
  lines[9] = paste("    2    1    1",
           sprintf("%4d", length(wnom_list)), "   2    5")
  lines[10] = "  5.9539  0.3463"
  writeLines(lines, "DW-NOMSTART.DAT")
}

write_input_files = function(rc_list, wnom_list) {
  # write the data files needed to run DW-NOMINATE
  cat("Writing input files...\n")
  write_rc_data_file(rc_list)
  write_transposed_rc_data_file(rc_list)
  write_leg_file(rc_list, wnom_list)
  write_bill_file(rc_list, wnom_list)
  write_session_file(rc_list, wnom_list)
  write_start_file(rc_list, wnom_list)
}

read_output_files = function(party_dict) {
  # column names based on DW-NOMINATE fortran code and info found
  # here: http://voteview.com/rohde.htm
  nas = c("**", "***", "****", "*****", "******",
      "*******", "************")
  legnames = c("session", "ID", "stateID", "district",
      "state", "partyID", "name", "coord1D", "coord2D",
      "se1D", "se2D", "var1D", "var2D", "loglikelihood",
      "loglikelihood_check", "numVotes", "numVotes_check",
      "numErrors", "numErrors_check", "GMP", "GMP_check")
  format1 = c("I4", "I6", "I3", "I2", "X", "A7", "X",
      "I4", "X", "A11", "6F7", "2F12", "4I5", "2F7")
  legs = read.fortran("legislator_output.dat", format1,
                      col.names = legnames, na.strings = nas)

  # get rid of first 3 iterations
  legs = legs[(3 * nrow(legs) / 4 + 1):nrow(legs), ]

  legs$party = names(party_dict)[legs$partyID]

  rcnames = c("session", "ID", "midpoint1D", "spread1D",
      "midpoint2D", "spread2D")
  format2 = c("I3", "I5", "4F7")
  rcs = read.fortran("rollcall_output.dat", format2,
      col.names = rcnames, na.strings = nas)

  # get rid of early iterations
  rcs = rcs[(3 * nrow(rcs) / 4 + 1):nrow(rcs), ]

  nas = which(rcs$spread1D == 0 & rcs$midpoint1D == 0 &
                rcs$spread2D == 0 & rcs$midpoint2D == 0)
  rcs[nas, 3:6] = NA

  list(legislators = legs, rollcalls = rcs)
}

#' Run DW-NOMINATE
#'
#' @useDynLib dwnominate dwnom
#' @param rc_list A list of \code{rollcall} objects from the
#'   \code{pscl} package, in chronological order.
#' @param wnom_list a list of W-NOMINATE results (class
#'   \code{nomObject}) from the \code{wnominate} package corresponding
#'   to the rollcall objects in \code{rc_list}. If no W-NOMINATE
#'   results are provided, W-NOMINATE will be run to get starting
#'   values for DW-NOMINATE.
#' @return A list of legislator and rollcall results of class \code{dwnominate}.
#' @examples
#' # US Senate data from voteview.com
#' data(senate)
#' results = dwnominate(senate)
#' plot(results)
#' @export
dwnominate = function(rc_list, wnom_list = NA) {
  if (length(rc_list) < 2)
    stop("rc_list must contain at least 2 rollcall objects")
  
  parties = unique(unlist(lapply(rc_list,
      function(x) x$legis.data$party)))
  party_dict = setNames(1:length(parties), parties)
  
  for (n in 1:length(rc_list)) {
    rc_list[[n]]$legis.data$party =
      party_dict[as.character(rc_list[[n]]$legis.data$party)]
  }
  if (is.na(wnom_list)[1]) {
    cat("Running W-NOMINATE to get starting values...")
    wnom_list = lapply(rc_list,
        function(x) wnominate::wnominate(x, polarity = 1:2))
  }
  for (n in 2:length(rc_list)) {
    id = ifelse("icpsrLegis" %in% names(rc_list[[n]]$legis.data),
                "icpsrLegis", "ID")
    # trying to keep all W-NOMINATE results oriented in the same
    # direction
    rc0 = rc_list[[n - 1]]
    rc = rc_list[[n]]
    wnom0 = wnom_list[[n - 1]]
    wnom = wnom_list[[n]]
    # 1st dimension
    low_ids = rc0$legis.data[wnom0$legislators$coord1D < 0, id]
    high_ids = rc0$legis.data[wnom0$legislators$coord1D > 0, id]
    mean_low = mean(
        wnom$legislators$coord1D[rc$legis.data[, id] %in%
                                 low_ids], na.rm = T)
    mean_high = mean(
        wnom$legislators$coord1D[rc$legis.data[, id] %in%
                                 high_ids], na.rm = T)
    if (mean_high < mean_low)
      wnom_list[[n]]$legislators$coord1D =
        -wnom_list[[n]]$legislators$coord1D
    # second dimension
    low_ids = rc0$legis.data[wnom0$legislators$coord2D < 0, id]
    high_ids = rc0$legis.data[wnom0$legislators$coord2D > 0, id]
    mean_low = mean(
        wnom$legislators$coord2D[rc$legis.data[, id] %in%
                                 low_ids], na.rm = T)
    mean_high = mean(
        wnom$legislators$coord2D[rc$legis.data[, id] %in%
                                 high_ids], na.rm = T)
    if (mean_high < mean_low)
      wnom_list[[n]]$legislators$coord2D =
        -wnom_list[[n]]$legislators$coord2D
  }
  
  write_input_files(rc_list, wnom_list)
  
  # run DW-NOMINATE
  # change line 40 of DW-NOMINATE.FOR !!
  nomstart = file.path(getwd(), "DW-NOMSTART.DAT")
  start = Sys.time()
  .Fortran("dwnom")
  runtime = Sys.time() - start
  units(runtime) = "mins"
  cat(paste("DW-NOMINATE took", round(runtime, 1),
            "minutes.\n"))
  
  # get the results
  results = read_output_files(party_dict)
  results$wnom_list = wnom_list
  results$rc_list = rc_list
  class(results) = "dwnominate"
  results
}

#' Plot party means over time
#'
#' @param dw_results An object returned by \code{dwnominate()}.
#' @examples
#' # US Senate data from voteview.com
#' data(senate)
#' results = dwnominate(senate)
#' plot(results)
#' @export
plot.dwnominate = function(dw_results) {
  pch = 18
  legs = dw_results$legislators
  uniq_parties = unique(legs$party)
  col_dict = setNames(rainbow(length(uniq_parties)),
      uniq_parties)
  ts1 = aggregate(legs$coord1D,
      by = list(legs$session, legs$party), FUN = mean, na.rm = T)
  plot(ts1$Group.1, ts1$x, ylim = c(-1, 1), type = "n",
       main = "DW-NOMINATE 1st Dimension",
       ylab = "Party Means", xlab = "Session")
  grid(NA, NULL, col = "#666666")
  for (party in unique(ts1$Group.2)) {
    ts2 = ts1[ts1$Group.2 == party, ]
    points(ts2$Group.1, ts2$x, col = col_dict[party],
           type = "o", pch = pch)
  }
  legend("topleft", names(col_dict), fill = col_dict,
         bg = "white", horiz = T)
}
