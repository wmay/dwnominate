# an interface to Keith Poole's DW-NOMINATE FORTRAN77 program.

# limitations:

# 600 legislators / session

# 3600 votes / session

write_rc_data_file = function(rc_list) {
  all_lines = vector()
  session = 1
  for (n in 1:length(rc_list)) {
    rc = rc_list[[n]]
    votes = rc$votes
    sessions = sprintf("%4.0f", session)
    leg_ids = sprintf("%5.0f", rc$legis.data$ID)
    state_num = sprintf("%2.0f", 1)
    district = sprintf("%1.0f", 0)
    state_name = sprintf("%-7s", "Vermont")
    parties = sprintf("%4d", rc$legis.data[["party"]])
    leg_names = sprintf("%-11.11s",
        gsub("[^A-Za-z -]", "", rownames(rc$votes)))
    
    # a dictionary to translate votes into the appropriate numbers
    # required by DW-NOMINATE
    vote_dict = setNames(c(1, 6, 9, 0), c("yes", "no", "other", "NA"))
    votes[is.na(votes)] = "NA"
    # instead of doing that vote dictionary, replace text with nums
    # directly:

    # a vector of very long lists of numbers, one for each legislator
    vote_nums = apply(votes, 1, FUN = function(x) paste(vote_dict[x],
                                    collapse = ""))

    # the lines to be written to the file
    lines = paste(sessions, leg_ids, state_num, district, state_name,
        parties, paste0(leg_names, vote_nums))
    all_lines = c(all_lines,  lines)
    session = session + 1
  }
  
  writeLines(all_lines, "rollcall_matrix.vt3")
}

write_transposed_rc_data_file = function(rc_list) {
  all_lines = vector()
  session = 1
  for (n in 1:length(rc_list)) {
    rc = rc_list[[n]]
    votes = rc$votes
    vote_ids = sprintf("%4.0f", 1:ncol(rc$votes))
    sessions = sprintf("%4.0f", session)
    
    # a dictionary to translate votes into the appropriate numbers
    # required by DW-NOMINATE
    vote_dict = setNames(c(1, 6, 9, 0), c("yes", "no", "other", "NA"))
    votes[is.na(votes)] = "NA"
    # instead of doing that vote dictionary, replace text with nums
    # directly:

    # a vector of very long lists of numbers, one for each legislator
    vote_nums = apply(votes, 2, FUN = function(x) paste0(vote_dict[x],
                                    collapse = ""))

    # the lines to be written to the file
    lines = paste(sessions, vote_ids, vote_nums)
    all_lines = c(all_lines, lines)
    session = session + 1
  }
  writeLines(all_lines, "transposed_rollcall_matrix.vt3")
}

write_leg_file = function(rc_list, wnom_list) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    wnom = wnom_list[[session]]
    sessions = sprintf("%4d", session)
    leg_ids = sprintf("%5d", rc$legis.data$ID)
    state_num = sprintf("%2d", 1)
    district = sprintf("%1d", 0)
    state_name = sprintf("%-7s", "Vermont")
    parties = sprintf("%4d", rc$legis.data[["party"]])
    leg_names = sprintf("%-11.11s",
        gsub("[^A-Za-z -]", "", rownames(rc$votes)))
    # DW-NOMINATE does not like NA's
    wnom$legislators[is.na(wnom$legislators)] = 0
    coord1 = sprintf("%6.3f", wnom$legislators$coord1D)
    coord2 = sprintf("%6.3f", wnom$legislators$coord2D)
    votes = sprintf("%4d", rowSums(wnom$legislators[, 2:5]))
    errors = sprintf("%4d",
        rowSums(wnom$legislators[, c("wrongYea", "wrongNay")]))
    gmp = sprintf("%6.3f", wnom$legislators$GMP)

    # the lines to be written to the file
    lines = paste(sessions, leg_ids, state_num, district, state_name,
        parties, leg_names, coord1, coord2,
        " 0.000  0.000  0.000  0.000     0.00000     0.00000",
        votes, votes, errors, errors, gmp, gmp)
    all_lines = c(all_lines, lines)
  }
  writeLines(all_lines, "legislator_input.dat")
}

write_bill_file = function(rc_list, wnom_list) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    wnom = wnom_list[[session]]
    sessions = sprintf("%3d", session)
    bill_ids = sprintf("%4d", 1:ncol(rc$votes))
    # DW-NOMINATE does not like NA's
    wnom$rollcalls[is.na(wnom$rollcalls)] = 0
    spread1 = sprintf("%6.3f", wnom$rollcalls$spread1D)
    spread2 = sprintf("%6.3f", wnom$rollcalls$spread2D)
    mid1 = sprintf("%6.3f", wnom$rollcalls$midpoint1D)
    mid2 = sprintf("%6.3f", wnom$rollcalls$midpoint2D)

    # the lines to be written to the file
    lines = paste(sessions, bill_ids, spread1, mid1, spread2, mid2)
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
  lines = file.path(getwd(), filenames)
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
  legwidths = c(4, 6, 3, 2, 8, 5, 12, 7, 7,
      7, 7, 7, 7, 12, 12, 5, 5, 5, 5, 7, 7)
  # column names based on info found here:
  # http://voteview.com/rohde.htm
  legnames = c("session", "ID", "stateID", "districtID",
      "state", "partyID", "name", "coord1D", "coord2D",
      "se1D", "se2D", "var1D", "var2D", "loglikelihood",
      "loglikelihood_check", "numVotes", "numVotes_check",
      "numErrors", "numErrors_check", "GMP", "GMP_check")
  legs = read.fwf("legislator_output.dat", widths = legwidths,
      col.names = legnames)

  # get rid of first 3 iterations
  legs = legs[(3 * nrow(legs) / 4 + 1):nrow(legs), ]
  if (class(legs$var2D) == "factor")
    legs$var2D = as.numeric(as.character(legs$var2D))

  legs$party = names(party_dict)[legs$partyID]

  rcwidths = c(3, 5, 7, 7, 7, 7)
  rcnames = c("session", "ID", "midpoint1D", "spread1D",
      "midpoint2D", "spread2D")
  rcs = read.fwf("rollcall_output.dat", widths = rcwidths,
      col.names = rcnames)

  # get rid of early iterations
  rcs = rcs[(3 * nrow(rcs) / 4 + 1):nrow(rcs), ]

  nas = which(rcs$spread1D == 0 & rcs$midpoint1D == 0 &
                rcs$spread2D == 0 & rcs$midpoint2D == 0)
  rcs$midpoint1D[nas] = NA
  rcs$spread1D[nas] = NA
  rcs$midpoint2D[nas] = NA
  rcs$spread2D[nas] = NA

  list(legislators = legs, rollcalls = rcs)
}

dwnominate = function(rc_list, wnom_list = NA) {
  if (length(rc_list) < 2)
    stop("rc_list must contain at least 2 rollcall objects")
  
  parties = unique(unlist(lapply(rc_list,
      function(x) x$legis.data$party)))
  party_dict = setNames(1:length(parties), parties)
  
  for (n in 1:length(rc_list)) {
    rc_list[[n]]$legis.data$party =
      party_dict[rc_list[[n]]$legis.data$party]
  }
  if (is.na(wnom_list)[1]) {
    cat("Running W-NOMINATE to get starting values...")
    wnom_list = lapply(rc_list,
        function(x) wnominate(x, polarity = 1:2))
  }
  for (n in 2:length(rc_list)) {
    # trying to keep all W-NOMINATE results oriented in the same
    # direction
    rc0 = rc_list[[n - 1]]
    rc = rc_list[[n]]
    wnom0 = wnom_list[[n - 1]]
    wnom = wnom_list[[n]]
    # 1st dimension
    low_ids = rc0$legis.data$ID[wnom0$legislators$coord1D < 0]
    high_ids = rc0$legis.data$ID[wnom0$legislators$coord1D > 0]
    mean_low = mean(
        wnom$legislators$coord1D[rc$legis.data$ID %in%
                                 low_ids], na.rm = T)
    mean_high = mean(
        wnom$legislators$coord1D[rc$legis.data$ID %in%
                                 high_ids], na.rm = T)
    if (mean_high < mean_low)
      wnom$legislators$coord1D = -wnom$legislators$coord1D
    # second dimension
    low_ids = rc0$legis.data$ID[wnom0$legislators$coord2D < 0]
    high_ids = rc0$legis.data$ID[wnom0$legislators$coord2D > 0]
    mean_low = mean(
        wnom$legislators$coord2D[rc$legis.data$ID %in%
                                 low_ids], na.rm = T)
    mean_high = mean(
        wnom$legislators$coord2D[rc$legis.data$ID %in%
                                 high_ids], na.rm = T)
    if (mean_high < mean_low)
      wnom$legislators$coord2D = -wnom$legislators$coord2D
  }
  
  write_input_files(rc_list, wnom_list)
  
  # run DW-NOMINATE
  # change line 40 of DW-NOMINATE.FOR !!
  nomstart = file.path(getwd(), "DW-NOMSTART.DAT")
  start = Sys.time()
  .Fortran("dwnominate", startfile = nomstart)
  runtime = Sys.time() - start
  units(runtime) = "mins"
  cat(paste("DW-NOMINATE took", round(runtime, 2),
            "minutes.\n"))
  
  # get the results
  read_output_files(party_dict)
}
